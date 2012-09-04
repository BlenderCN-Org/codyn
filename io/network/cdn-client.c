#include "cdn-client.h"
#include "cdn-network-thread.h"
#include <codyn/cdn-selector.h>
#include <codyn/cdn-math.h>
#include <codyn/cdn-io.h>
#include <string.h>
#include <codyn/cdn-debug.h>

#define CDN_CLIENT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_CLIENT, CdnClientPrivate))

typedef enum
{
	VARIABLE_TYPE_VARIABLE,
	VARIABLE_TYPE_SELECTOR,
} VariableType;

typedef enum
{
	VALUE_TYPE_VALUE,
	VALUE_TYPE_EXPRESSION,
} ValueType;

typedef enum
{
	MESSAGE_TYPE_SET,
	MESSAGE_TYPE_HEADER,
	MESSAGE_TYPE_BINARY_MODE
} MessageType;

typedef struct
{
	MessageType type;
} Message;

typedef struct
{
	union
	{
		Message message;
		MessageType type;
	};

	union
	{
		CdnVariable *variable;
		gchar *selector;
	};

	union
	{
		struct
		{
			gdouble *value;
			gint numr;
			gint numc;
		} value;

		gchar *expression;
	};

	VariableType variable_type;
	ValueType value_type;
} MessageSet;

typedef struct
{
	gchar *name;
	gint   numr;
	gint   numc;
} HeaderVariable;

typedef enum
{
	HEADER_DIRECTION_IN,
	HEADER_DIRECTION_OUT
} HeaderDirection;

typedef struct
{
	union
	{
		Message message;
		MessageType type;
	};

	HeaderDirection direction;
	GSList *variables;
} MessageHeader;

struct _CdnClientPrivate
{
	GSocket *socket;
	GSocketAddress *address;
	CdnNode *node;
	gchar *bytes;
	gssize num_bytes;
	gssize offset;

	GPtrArray *in_variables;
	GPtrArray *out_variables;
	GHashTable *in_variables_map;

	GSList *messages;
	GMutex *message_mutex;

	CdnIoMode io_mode;
	gdouble throttle;
	GTimer *throttle_timer;
	GString *outbuf;
	GOutputStream *boutbuf;

	gint *input_map;
	gint num_input_map;

	gint *output_map;
	gint num_output_map;

	CdnCompileContext *cctx;

	guint binary_mode : 1;
	guint isdatagram : 1;
};

G_DEFINE_TYPE (CdnClient, cdn_client, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_NODE,
	PROP_SOCKET,
	PROP_BINARY_MODE,
	PROP_IO_MODE,
	PROP_THROTTLE,
	PROP_ADDRESS
};

enum
{
	CLOSED,
	NUM_SIGNALS
};

static guint signals[NUM_SIGNALS] = {0,};

static void
cdn_client_finalize (GObject *object)
{
	CdnClient *self;

	self = CDN_CLIENT (object);

	g_ptr_array_free (self->priv->in_variables, TRUE);
	g_hash_table_destroy (self->priv->in_variables_map);

	g_ptr_array_free (self->priv->out_variables, TRUE);

	if (self->priv->node)
	{
		g_object_unref (self->priv->node);
	}

	if (self->priv->socket)
	{
		g_socket_close (self->priv->socket, NULL);
		g_object_unref (self->priv->socket);
	}

	if (self->priv->address)
	{
		g_object_unref (self->priv->address);
	}

	if (self->priv->throttle_timer)
	{
		g_timer_destroy (self->priv->throttle_timer);
	}

	if (self->priv->outbuf)
	{
		g_string_free (self->priv->outbuf, TRUE);
	}

	if (self->priv->boutbuf)
	{
		g_object_unref (self->priv->boutbuf);
	}

	if (self->priv->cctx)
	{
		g_object_unref (self->priv->cctx);
	}

	g_free (self->priv->input_map);
	g_free (self->priv->output_map);

	G_OBJECT_CLASS (cdn_client_parent_class)->finalize (object);
}

static void
cdn_client_set_property (GObject      *object,
                         guint         prop_id,
                         const GValue *value,
                         GParamSpec   *pspec)
{
	CdnClient *self = CDN_CLIENT (object);

	switch (prop_id)
	{
		case PROP_NODE:
			self->priv->node = g_value_dup_object (value);
			break;
		case PROP_SOCKET:
			self->priv->socket = g_value_dup_object (value);
			break;
		case PROP_BINARY_MODE:
			self->priv->binary_mode = g_value_get_boolean (value);
			break;
		case PROP_IO_MODE:
			self->priv->io_mode = g_value_get_flags (value);
			break;
		case PROP_THROTTLE:
			self->priv->throttle = g_value_get_double (value);
			break;
		case PROP_ADDRESS:
			self->priv->address = g_value_dup_object (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_client_get_property (GObject    *object,
                         guint       prop_id,
                         GValue     *value,
                         GParamSpec *pspec)
{
	CdnClient *self = CDN_CLIENT (object);

	switch (prop_id)
	{
		case PROP_NODE:
			g_value_set_object (value, self->priv->node);
			break;
		case PROP_SOCKET:
			g_value_set_object (value, self->priv->socket);
			break;
		case PROP_BINARY_MODE:
			g_value_set_boolean (value, self->priv->binary_mode);
			break;
		case PROP_IO_MODE:
			g_value_set_flags (value, self->priv->io_mode);
			break;
		case PROP_THROTTLE:
			g_value_set_double (value, self->priv->throttle);
			break;
		case PROP_ADDRESS:
			g_value_set_object (value, self->priv->address);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
header_variable_free (HeaderVariable *v)
{
	g_free (v->name);
	g_slice_free (HeaderVariable, v);
}

static HeaderVariable *
header_variable_new (gchar              *name,
                     CdnDimension const *dim)
{
	HeaderVariable *ret;

	ret = g_slice_new (HeaderVariable);

	ret->name = name;
	ret->numr = dim->rows;
	ret->numc = dim->columns;

	return ret;
}

static void
message_set_free (MessageSet *msg)
{
	if (!msg)
	{
		return;
	}

	if (msg->variable_type == VARIABLE_TYPE_VARIABLE)
	{
		g_object_unref (msg->variable);
	}
	else
	{
		g_free (msg->selector);
	}

	if (msg->value_type == VALUE_TYPE_VALUE)
	{
		g_free (msg->value.value);
	}
	else
	{
		g_free (msg->expression);
	}

	g_slice_free (MessageSet, msg);
}

static Message *
message_set_new (CdnVariable *variable,
                 gchar const *selector,
                 gchar const *expression,
                 gdouble     *value,
                 gint         numr,
                 gint         numc)
{
	MessageSet *ret;

	if ((!variable && !selector) || (!expression && !value))
	{
		return NULL;
	}

	ret = g_slice_new (MessageSet);

	ret->type = MESSAGE_TYPE_SET;

	if (!variable)
	{
		ret->selector = g_strdup (selector);
		ret->variable_type = VARIABLE_TYPE_SELECTOR;
	}
	else
	{
		ret->variable = g_object_ref (variable);
		ret->variable_type = VARIABLE_TYPE_VARIABLE;
	}

	if (expression)
	{
		ret->expression = g_strdup (expression);

		ret->value_type = VALUE_TYPE_EXPRESSION;
	}
	else
	{
		ret->value.value = value;
		ret->value.numr = numr;
		ret->value.numc = numc;

		ret->value_type = VALUE_TYPE_VALUE;
	}

	return (Message *)ret;
}

static void
message_header_free (MessageHeader *message)
{
	while (message->variables)
	{
		header_variable_free (message->variables->data);

		message->variables = g_slist_delete_link (message->variables,
		                                          message->variables);
	}

	g_slice_free (MessageHeader, message);
}

static void
message_free (Message *message)
{
	switch (message->type)
	{
		case MESSAGE_TYPE_HEADER:
			message_header_free ((MessageHeader *)message);
		break;
		case MESSAGE_TYPE_SET:
			message_set_free ((MessageSet *)message);
		break;
		default:
			g_slice_free (Message, message);
		break;
	}
}

static Message *
message_new (MessageType type)
{
	Message *ret;

	ret = g_slice_new0 (Message);

	ret->type = type;
	return ret;
}

static Message *
message_header_new (HeaderDirection direction)
{
	MessageHeader *ret;

	ret = g_slice_new0 (MessageHeader);
	ret->type = MESSAGE_TYPE_HEADER;

	ret->direction = direction;

	return (Message *)ret;
}

static void
push_message (CdnClient *client,
              Message   *message)
{
	if (!message || !(client->priv->io_mode & CDN_IO_MODE_INPUT))
	{
		message_free (message);
		return;
	}

	g_mutex_lock (client->priv->message_mutex);

	if (message->type == MESSAGE_TYPE_HEADER)
	{
		client->priv->messages = g_slist_append (client->priv->messages,
		                                         message);
	}
	else
	{
		client->priv->messages = g_slist_prepend (client->priv->messages,
		                                          message);
	}

	g_mutex_unlock (client->priv->message_mutex);
}

static gboolean
is_space (gunichar c)
{
	return c == ' ' || c == '\t';
}

static gboolean
ascii_skip_while (CdnClient *client,
                  gssize    *start,
                  gboolean (*match_func) (gunichar))
{
	while (*start < client->priv->offset &&
	       match_func (client->priv->bytes[*start]))
	{
		++*start;
	}

	return (*start < client->priv->offset && client->priv->bytes[*start]);
}

static gboolean
ascii_skip_spaces (CdnClient *client,
                   gssize    *start)
{
	return ascii_skip_while (client, start, is_space);
}

static gboolean
is_index (gchar c)
{
	return g_ascii_isdigit (c);
}

static gboolean
ascii_read_index (CdnClient *client,
                  gssize    *start,
                  gint      *idx)
{
	gssize s = *start;

	*idx = 0;

	while (s < client->priv->offset &&
	       is_index (client->priv->bytes[s]))
	{
		*idx = *idx * 10 + client->priv->bytes[s] - '0';
		++s;
	}

	if (s == *start)
	{
		return FALSE;
	}
	else
	{
		*start = s;
		return TRUE;
	}
}

static gboolean
ascii_read_identifier (CdnClient  *client,
                       gssize     *start,
                       gchar     **ident)
{
	GString *ret;
	gchar *ptr;
	gunichar c;

	if (*start >= client->priv->offset)
	{
		return FALSE;
	}

	ret = g_string_sized_new (128);
	ptr = client->priv->bytes + *start;

	while (*start < client->priv->offset)
	{
		c = g_utf8_get_char (ptr);

		if (!(g_unichar_isalnum (c) || c == '_'))
		{
			break;
		}

		ret = g_string_append_c (ret, c);

		ptr = g_utf8_next_char (ptr);
		*start = ptr - client->priv->bytes;
	}

	c = g_utf8_get_char (ptr);

	if (!is_space (c) && c != '\n')
	{
		g_string_free (ret, TRUE);
		return FALSE;
	}
	else
	{
		*ident = g_string_free (ret, FALSE);
		return TRUE;
	}
}

static gboolean
ascii_read_selector (CdnClient  *client,
                     gssize     *start,
                     gchar     **selector)
{
	GString *ret;
	gchar *ptr;
	gboolean inesc = FALSE;
	gboolean endedesc = FALSE;

	if (*start >= client->priv->offset)
	{
		return FALSE;
	}

	ret = g_string_sized_new (128);
	ptr = client->priv->bytes + *start;

	if (g_utf8_get_char (ptr) == '\'')
	{
		inesc = TRUE;
		ptr = g_utf8_next_char (ptr);
		*start = ptr - client->priv->bytes;
	}

	while (*start < client->priv->offset)
	{
		gunichar c;

		c = g_utf8_get_char (ptr);

		if (!c || (!inesc && (is_space (c) || c == '\n')))
		{
			break;
		}

		if (!inesc || c != '\'')
		{
			ret = g_string_append_c (ret, c);
		}

		ptr = g_utf8_next_char (ptr);
		*start = ptr - client->priv->bytes;

		if (inesc && c == '\'')
		{
			endedesc = TRUE;
			break;
		}
	}

	if (inesc && !endedesc)
	{
		g_string_free (ret, TRUE);
		return FALSE;
	}

	*selector = g_string_free (ret, FALSE);
	return TRUE;
}

static gboolean
ascii_read_value_expression (CdnClient  *client,
                             gssize     *start,
                             gchar     **expression)
{
	GString *ret;
	gchar *ptr;
	gboolean endedesc = FALSE;

	if (*start >= client->priv->offset ||
	    g_utf8_get_char (client->priv->bytes + *start) != '\'')
	{
		return FALSE;
	}

	ret = g_string_sized_new (128);
	ptr = g_utf8_next_char (client->priv->bytes + *start);
	*start = ptr - client->priv->bytes;

	while (*start < client->priv->offset)
	{
		gunichar c;

		c = g_utf8_get_char (ptr);

		if (!c)
		{
			break;
		}

		if (c != '\'')
		{
			ret = g_string_append_c (ret, c);
		}

		ptr = g_utf8_next_char (ptr);
		*start = ptr - client->priv->bytes;

		if (c == '\'')
		{
			endedesc = TRUE;
			break;
		}
	}

	if (!endedesc)
	{
		g_string_free (ret, TRUE);
		return FALSE;
	}

	*expression = g_string_free (ret, FALSE);
	return TRUE;
}

static gboolean
ascii_read_value (CdnClient  *client,
                  gssize     *start,
                  gdouble    *value,
                  gboolean   (*sep_func)(gunichar))
{
	gboolean hasdot = FALSE;
	gboolean hase = FALSE;
	gboolean aftere = FALSE;
	gunichar c;
	gchar const *ptr;

	if (*start >= client->priv->offset)
	{
		return FALSE;
	}

	if (!sep_func)
	{
		sep_func = is_space;
	}

	ptr = client->priv->bytes + *start;

	c = g_utf8_get_char (client->priv->bytes + *start);
	hasdot = c == '.';

	if (!g_unichar_isdigit (c) && c != '-' && c != '.')
	{
		gchar *ident;
		gboolean found = FALSE;

		if (!ascii_read_identifier (client, start, &ident))
		{
			return FALSE;
		}

		*value = cdn_math_constant_lookup (ident, &found);
		g_free (ident);

		return found;
	}

	*start = g_utf8_next_char (client->priv->bytes + *start) - client->priv->bytes;

	if (*start >= client->priv->offset)
	{
		return FALSE;
	}

	while (*start < client->priv->offset)
	{
		c = g_utf8_get_char (client->priv->bytes + *start);

		if (!c || sep_func (c) || c == '\n')
		{
			break;
		}

		if (c == '.')
		{
			if (hasdot || hase)
			{
				return FALSE;
			}

			hasdot = TRUE;
		}
		else if (c == 'e' || c == 'E')
		{
			if (hase)
			{
				return FALSE;
			}

			hase = TRUE;
		}
		else if (c == '-')
		{
			if (!hase || aftere)
			{
				return FALSE;
			}

			aftere = TRUE;
		}
		else if (g_unichar_isdigit (c))
		{
			if (hase)
			{
				aftere = TRUE;
			}
		}
		else
		{
			return FALSE;
		}

		*start = g_utf8_next_char (client->priv->bytes + *start) -
		         client->priv->bytes;
	}

	if ((!hase || aftere) && ptr != client->priv->bytes + *start)
	{
		gchar *cp = g_strndup (ptr, (client->priv->bytes + *start) - ptr);
		gchar *end;

		*value = g_ascii_strtod (cp, &end);
		return !*end;
	}
	else
	{
		return FALSE;
	}
}

static gboolean
is_row_sep (gunichar c)
{
	return is_space (c) || c == ',';
}

static gboolean
is_row_value_end (gunichar c)
{
	return is_row_sep (c) || c == ';' || c == ']';
}

static gboolean
ascii_read_value_row (CdnClient *client,
                      gssize    *start,
                      GArray    *value,
                      gint      *numr,
                      gint      *numc)
{
	gint n = 0;
	gboolean skipped;
	gboolean first = TRUE;

	while (*start < client->priv->offset)
	{
		gunichar c;
		gdouble v;

		skipped = ascii_skip_while (client, start, is_row_sep);

		c = g_utf8_get_char (client->priv->bytes + *start);

		if (c == ';' || c == ']')
		{
			// End of the line
			if (*numr != 0 && n != *numc)
			{
				return FALSE;
			}
			else
			{
				++*numr;
				*numc = n;

				return TRUE;
			}
		}

		if (!skipped && !first)
		{
			return FALSE;
		}

		first = FALSE;

		if (!ascii_read_value (client, start, &v, is_row_value_end))
		{
			return FALSE;
		}

		g_array_append_val (value, v);
		++n;
	}

	return FALSE;
}

static gboolean
ascii_read_value_matrix (CdnClient  *client,
                         gssize     *start,
                         gdouble   **value,
                         gint       *numr,
                         gint       *numc)
{
	// Read simple numerical/math constant matrix value
	gchar *ptr;
	GArray *ret;

	if (*start >= client->priv->offset ||
	    g_utf8_get_char (client->priv->bytes + *start) != '[')
	{
		return FALSE;
	}

	ret = g_array_sized_new (FALSE, TRUE, sizeof (gdouble), 20);

	ptr = g_utf8_next_char (client->priv->bytes + *start);
	*start = ptr - client->priv->bytes;

	*numr = 0;
	*numc = 0;

	while (TRUE)
	{
		gunichar c;

		if (!ascii_read_value_row (client, start, ret, numr, numc))
		{
			g_array_free (ret, TRUE);
			return FALSE;
		}

		c = g_utf8_get_char (client->priv->bytes + *start);

		if (c == ';' || c == ']')
		{
			ptr = g_utf8_next_char (client->priv->bytes + *start);
			*start = ptr - client->priv->bytes;

			if (c == ']')
			{
				break;
			}
		}
		else
		{
			g_array_free (ret, TRUE);
			return FALSE;
		}
	}

	*value = (gdouble *)g_array_free (ret, FALSE);
	return TRUE;
}

static gint
lookup_index_map (gint *map,
                  gint  num_map,
                  gint  idx)
{
	if (map)
	{
		if (idx < num_map)
		{
			return map[idx];
		}
		else
		{
			return -1;
		}
	}
	else
	{
		return idx;
	}
}

static gboolean
process_set_ascii (CdnClient *client,
                   gssize    *start,
                   gboolean   simple_mode)
{
	gboolean ret = TRUE;
	gint idx = 0;

	while (*start < client->priv->offset)
	{
		CdnVariable *v = NULL;
		gchar *selector = NULL;
		gchar *ident = NULL;
		gchar *expression = NULL;
		gdouble *value = NULL;
		gdouble singleval = 0;
		gint numr = 1;
		gint numc = 1;
		gssize s;

		s = *start;

		if (client->priv->bytes[s] == '\n')
		{
			++*start;
			break;
		}

		if (!ascii_skip_spaces (client, &s) && !simple_mode)
		{
			ret = FALSE;
			break;
		}

		// Allow trailing whitespace
		if (client->priv->bytes[s] == '\n')
		{
			*start = s + 1;
			break;
		}

		if (simple_mode)
		{
			gint midx;

			midx = lookup_index_map (client->priv->input_map,
			                         client->priv->num_input_map,
			                         idx);

			if (midx < client->priv->in_variables->len)
			{
				v = g_ptr_array_index (client->priv->in_variables,
				                       midx);
			}

			++idx;
		}
		else
		{
			if (ascii_read_index (client, &s, &idx))
			{
				ret = TRUE;

				idx = lookup_index_map (client->priv->input_map,
				                        client->priv->num_input_map,
				                        idx);

				// This is an index
				if (idx < client->priv->in_variables->len)
				{
					v = g_ptr_array_index (client->priv->in_variables,
					                       idx);
				}
			}
			else if (ascii_read_identifier (client, &s, &ident))
			{
				// This is a simple child
				v = g_hash_table_lookup (client->priv->in_variables_map,
				                         ident);

				g_free (ident);
			}
			else if (!ascii_read_selector (client, &s, &selector))
			{
				ret = FALSE;
			}

			if (!ascii_skip_spaces (client, &s) && !simple_mode)
			{
				g_warning ("Failed to skip");
				ret = FALSE;
				break;
			}
		}

		if (!ret)
		{
			g_free (selector);
			break;
		}

		// Read the value
		if (!ascii_read_value_expression (client, &s, &expression) &&
		    !ascii_read_value_matrix (client, &s, &value, &numr, &numc) &&
		    !ascii_read_value (client, &s, &singleval, 0))
		{
			g_free (selector);
			break;
		}

		if (!value && !expression)
		{
			value = g_new (gdouble, 1);
			*value = singleval;
			numr = 1;
			numc = 1;
		}

		push_message (client, message_set_new (v,
		                                       selector,
		                                       expression,
		                                       value,
		                                       numr,
		                                       numc));

		*start = s;

		g_free (expression);
		g_free (selector);
	}

	return ret;
}

static gboolean
process_set_binary (CdnClient *client,
                    gssize    *start)
{
	GInputStream *inp;
	GDataInputStream *stream;
	guint32 i;
	GError *err = NULL;

	inp = g_memory_input_stream_new_from_data (client->priv->bytes + *start,
	                                           client->priv->offset - *start,
	                                           NULL);

	stream = g_data_input_stream_new (inp);

	guint32 numvar = g_data_input_stream_read_uint32 (stream, NULL, &err);

	if (err)
	{
		g_error_free (err);
		return FALSE;
	}

	for (i = 0; i < numvar; ++i)
	{
		CdnVariable *v;
		guint16 idx;
		CdnExpression *e;
		gdouble *ptr;
		CdnDimension dim;
		gint j;
		gint num;

		idx = g_data_input_stream_read_uint16 (stream, NULL, &err);

		if (err)
		{
			g_error_free (err);
			return FALSE;
		}

		if (idx >= client->priv->in_variables->len)
		{
			return FALSE;
		}

		v = g_ptr_array_index (client->priv->in_variables, idx);
		e = cdn_variable_get_expression (v);

		cdn_expression_get_dimension (e, &dim);
		num = cdn_dimension_size (&dim);
		ptr = g_new (gdouble, num);

		for (j = 0; j < num; ++j)
		{
			union
			{
				guint64 val;
				gdouble dval;
			} val;

			val.val = g_data_input_stream_read_uint64 (stream, NULL, &err);

			ptr[j] = val.dval;

			if (err)
			{
				g_free (ptr);

				g_error_free (err);
				return FALSE;
			}
		}

		cdn_expression_set_values (e,
		                           ptr,
		                           &dim);

		g_free (ptr);
	}

	*start += g_seekable_tell (G_SEEKABLE (inp));

	g_object_unref (stream);
	g_object_unref (inp);

	return TRUE;
}

static gboolean
process_header (CdnClient       *client,
                gssize          *start,
                HeaderDirection  direction)
{
	gboolean ret = TRUE;
	MessageHeader *message;

	message = (MessageHeader *)message_header_new (direction);

	while (*start < client->priv->offset)
	{
		CdnDimension dim = CDN_DIMENSION (1, 1);
		gchar *ident;
		gssize s;

		s = *start;

		if (client->priv->bytes[*start] == '\n')
		{
			++*start;
			break;
		}

		if (!ascii_skip_spaces (client, &s))
		{
			ret = FALSE;
			break;
		}

		// Allow trailing whitespace
		if (client->priv->bytes[*start] == '\n')
		{
			++*start;
			break;
		}

		if (!ascii_read_index (client, &s, &dim.rows))
		{
			ret = FALSE;
			break;
		}

		if (!ascii_skip_spaces (client, &s))
		{
			ret = FALSE;
			break;
		}

		if (!ascii_read_index (client, &s, &dim.columns))
		{
			ret = FALSE;
			break;
		}

		if (!ascii_skip_spaces (client, &s))
		{
			ret = FALSE;
			break;
		}

		if (!ascii_read_identifier (client, &s, &ident))
		{
			ret = FALSE;
			break;
		}

		message->variables = g_slist_prepend (message->variables,
		                                      header_variable_new (ident,
		                                                           &dim));

		*start = s;
	}

	if (ret)
	{
		message->variables = g_slist_reverse (message->variables);
		push_message (client, (Message *)message);
	}
	else
	{
		message_header_free (message);
	}

	return ret;
}

static gboolean
process_input_header (CdnClient *client,
                      gssize    *start)
{
	// Input negotation The other end of the connection is indicating
	// which variables it will be sending
	return process_header (client, start, HEADER_DIRECTION_IN);
}

static gboolean
process_output_header (CdnClient *client,
                       gssize    *start)
{
	// Input negotation The other end of the connection is indicating
	// which variables it will be receiving
	return process_header (client, start, HEADER_DIRECTION_OUT);
}

static gboolean
process_binary_mode (CdnClient *client,
                     gssize    *start)
{
	if (client->priv->bytes[*start] == '\n')
	{
		++*start;
		push_message (client, message_new (MESSAGE_TYPE_BINARY_MODE));
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

static void
buffer_copy (CdnClient *client,
             gssize     start)
{
	gssize i;
	gssize s = 0;

	for (i = start; i < client->priv->offset; ++i)
	{
		client->priv->bytes[s++] = client->priv->bytes[i];
	}

	client->priv->offset = start;
}

static void
process_messages (CdnClient *client)
{
	gssize start = 0;

	while (start < client->priv->offset)
	{
		gssize s = start + 1;
		gboolean ret;

		switch (*(client->priv->bytes + start))
		{
			case 'i':
				ret = process_input_header (client, &s);
			break;
			case 'o':
				ret = process_output_header (client, &s);
			break;
			case 'x':
				ret = process_set_binary (client, &s);
			break;
			case 's':
				ret = process_set_ascii (client, &s, FALSE);
			break;
			case 'b':
				ret = process_binary_mode (client, &s);
			break;
			default:
				--s;
				ret = process_set_ascii (client, &s, TRUE);
			break;
		}

		if (ret)
		{
			start = s;
		}
		else
		{
			// Fixup buffer here to last known start
			buffer_copy (client, start);
			break;
		}
	}

	client->priv->offset = 0;
}

static gboolean
recv_stream (CdnClient *client)
{
	gboolean hadit = FALSE;

	while (TRUE)
	{
		gssize numrecv = 0;
		GError *err = NULL;

		numrecv = g_socket_receive_with_blocking (client->priv->socket,
		                                          client->priv->bytes +
		                                          client->priv->offset,
		                                          client->priv->num_bytes -
		                                          client->priv->offset,
		                                          FALSE,
		                                          NULL,
		                                          &err);

		if (numrecv == -1 && g_error_matches (err,
		                                      G_IO_ERROR,
		                                      G_IO_ERROR_WOULD_BLOCK))
		{
			if (err)
			{
				g_error_free (err);
			}

			break;
		}
		else if (numrecv > 0)
		{
			client->priv->offset += numrecv;

			if (client->priv->offset >=
			    client->priv->num_bytes)
			{
				client->priv->num_bytes *= 2;

				client->priv->bytes =
					g_renew (gchar,
					         client->priv->bytes,
					         client->priv->num_bytes);
			}

			break;
		}
		else if (!hadit)
		{
			return FALSE;
		}
		else
		{
			break;
		}

		hadit = TRUE;
	}

	return TRUE;
}

static gboolean
recv_datagram (CdnClient *client)
{
	gssize numrecv;
	GError *err = NULL;

	numrecv = g_socket_receive_with_blocking (client->priv->socket,
	                                          client->priv->bytes,
	                                          client->priv->num_bytes,
	                                          FALSE,
	                                          NULL,
	                                          &err);

	if (numrecv == -1)
	{
		if (g_error_matches (err, G_IO_ERROR, G_IO_ERROR_WOULD_BLOCK))
		{
			g_error_free (err);

			return TRUE;
		}
		else
		{
			return FALSE;
		}
	}
	else if (numrecv == 0)
	{
		return FALSE;
	}
	else
	{
		client->priv->offset = numrecv;
	}

	return TRUE;
}

static gboolean
io_in (GSocket      *socket,
       GIOCondition  condition,
       CdnClient    *client)
{
	gboolean ret = TRUE;

	// This runs in a separate thread
	if (condition & G_IO_IN)
	{
		gboolean ret;

		if (!client->priv->isdatagram)
		{
			ret = recv_stream (client);
		}
		else
		{
			ret = recv_datagram (client);
		}

		process_messages (client);

		if (!ret)
		{
			CdnNetworkThread *t;

			t = cdn_network_thread_get_default ();

			cdn_network_thread_unregister (t, client->priv->socket);
			g_socket_close (client->priv->socket, NULL);
		}
	}

	return ret;
}

static void
cdn_client_constructed (GObject *object)
{
	CdnClient *client;
	GSList *vars;

	client = CDN_CLIENT (object);

	client->priv->num_bytes = 512;
	client->priv->bytes = g_new (gchar, client->priv->num_bytes);

	client->priv->message_mutex = g_mutex_new ();

	client->priv->in_variables_map = g_hash_table_new_full (g_str_hash,
	                                                        g_str_equal,
	                                                        (GDestroyNotify)g_free,
	                                                        (GDestroyNotify)g_object_unref);

	client->priv->in_variables = g_ptr_array_sized_new (20);
	g_ptr_array_set_free_func (client->priv->in_variables,
	                           (GDestroyNotify)g_object_unref);

	client->priv->out_variables = g_ptr_array_sized_new (20);
	g_ptr_array_set_free_func (client->priv->out_variables,
	                           (GDestroyNotify)g_object_unref);

	// Scan variables
	vars = cdn_object_get_variables (CDN_OBJECT (client->priv->node));

	while (vars)
	{
		CdnVariable *v = vars->data;

		if ((client->priv->io_mode & CDN_IO_MODE_INPUT) &&
		    cdn_variable_get_flags (v) & CDN_VARIABLE_FLAG_IN)
		{
			g_ptr_array_add (client->priv->in_variables,
			                 g_object_ref (v));

			g_hash_table_insert (client->priv->in_variables_map,
			                     g_strdup (cdn_variable_get_name (v)),
			                     g_object_ref (v));
		}

		if ((client->priv->io_mode & CDN_IO_MODE_OUTPUT) &&
		    cdn_variable_get_flags (v) & CDN_VARIABLE_FLAG_OUT)
		{
			g_ptr_array_add (client->priv->out_variables,
			                 g_object_ref (v));
		}

		vars = g_slist_delete_link (vars, vars);
	}

	client->priv->isdatagram =
		g_socket_get_socket_type (client->priv->socket) ==
		G_SOCKET_TYPE_DATAGRAM;

	client->priv->outbuf = g_string_sized_new (1024);

	cdn_network_thread_register (cdn_network_thread_get_default (),
	                             client->priv->socket,
	                             (GSocketSourceFunc)io_in,
	                             g_object_ref (client),
	                             (GDestroyNotify)g_object_unref);
}

static void
cdn_client_class_init (CdnClientClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_client_finalize;

	object_class->constructed = cdn_client_constructed;

	object_class->get_property = cdn_client_get_property;
	object_class->set_property = cdn_client_set_property;

	g_type_class_add_private (object_class, sizeof (CdnClientPrivate));

	signals[CLOSED] =
		g_signal_new ("closed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              0,
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE,
		              0);

	g_object_class_install_property (object_class,
	                                 PROP_NODE,
	                                 g_param_spec_object ("node",
	                                                      "Node",
	                                                      "Node",
	                                                      CDN_TYPE_NODE,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT_ONLY |
	                                                      G_PARAM_STATIC_STRINGS));

	g_object_class_install_property (object_class,
	                                 PROP_SOCKET,
	                                 g_param_spec_object ("socket",
	                                                      "Socket",
	                                                      "Socket",
	                                                      G_TYPE_SOCKET,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT_ONLY |
	                                                      G_PARAM_STATIC_STRINGS));

	g_object_class_install_property (object_class,
	                                 PROP_BINARY_MODE,
	                                 g_param_spec_boolean ("binary-mode",
	                                                       "Binary Mode",
	                                                       "Binary mode",
	                                                       FALSE,
	                                                       G_PARAM_READWRITE |
	                                                       G_PARAM_CONSTRUCT_ONLY |
	                                                       G_PARAM_STATIC_STRINGS));

	g_object_class_install_property (object_class,
	                                 PROP_IO_MODE,
	                                 g_param_spec_flags ("io-mode",
	                                                     "Io Mode",
	                                                     "Io mode",
	                                                     CDN_TYPE_IO_MODE,
	                                                     CDN_IO_MODE_INPUT,
	                                                     G_PARAM_READWRITE |
	                                                     G_PARAM_CONSTRUCT_ONLY |
	                                                     G_PARAM_STATIC_STRINGS));

	g_object_class_install_property (object_class,
	                                 PROP_THROTTLE,
	                                 g_param_spec_double ("throttle",
	                                                      "Throttle",
	                                                      "Throttle",
	                                                      0,
	                                                      G_MAXDOUBLE,
	                                                      0,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT_ONLY |
	                                                      G_PARAM_STATIC_STRINGS));

	g_object_class_install_property (object_class,
	                                 PROP_ADDRESS,
	                                 g_param_spec_object ("address",
	                                                      "Address",
	                                                      "Address",
	                                                      G_TYPE_SOCKET_ADDRESS,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT_ONLY |
	                                                      G_PARAM_STATIC_STRINGS));
}

static void
cdn_client_init (CdnClient *self)
{
	self->priv = CDN_CLIENT_GET_PRIVATE (self);
}

CdnClient *
cdn_client_new (CdnNode        *node,
                GSocket        *socket,
                GSocketAddress *addr,
                CdnIoMode       io_mode,
                gdouble         throttle)
{
	return g_object_new (CDN_TYPE_CLIENT,
	                     "node", node,
	                     "socket", socket,
	                     "address", addr,
	                     "io-mode", io_mode,
	                     "throttle", throttle,
	                     NULL);
}

static void
send_out_limit_ascii (CdnClient *client,
                      guint      limit)
{
	gint i;
	gchar numbuf[G_ASCII_DTOSTR_BUF_SIZE];
	guint lastlen = 0;
	guint soff = 0;

	if (client->priv->output_map)
	{
		g_string_append (client->priv->outbuf, "s ");
		soff = 2;
	}

	for (i = 0; i < client->priv->out_variables->len; ++i)
	{
		CdnVariable *v;
		gdouble const *vals;
		CdnDimension dim;
		gint mapped;

		mapped = lookup_index_map (client->priv->output_map,
		                           client->priv->num_output_map,
		                           i);

		if (mapped == -1)
		{
			continue;
		}

		if (limit > 0 && client->priv->outbuf->len >= limit)
		{
			g_socket_send_to (client->priv->socket,
			                  client->priv->address,
			                  client->priv->outbuf->str,
			                  lastlen,
			                  NULL,
			                  NULL);

			g_string_erase (client->priv->outbuf, soff, lastlen - 1);

			if (client->priv->outbuf->len > limit)
			{
				g_string_truncate (client->priv->outbuf, soff);
			}
		}
		else if (lastlen > soff)
		{
			client->priv->outbuf->str[lastlen - 1] = ' ';
		}

		lastlen = client->priv->outbuf->len;

		v = g_ptr_array_index (client->priv->out_variables, i);


		if (client->priv->output_map)
		{
			g_string_append_printf (client->priv->outbuf,
			                        "%u ", i);
		}

		vals = cdn_variable_get_values (v, &dim);

		if (cdn_dimension_is_one (&dim))
		{
			g_ascii_dtostr (numbuf,
			                G_ASCII_DTOSTR_BUF_SIZE,
			                vals[0]);

			g_string_append (client->priv->outbuf, numbuf);
		}
		else
		{
			gint r;
			gint c;
			gint i = 0;

			g_string_append_c (client->priv->outbuf, '[');

			for (r = 0; r < dim.rows; ++r)
			{
				if (r != 0)
				{
					g_string_append (client->priv->outbuf, "; ");
				}

				for (c = 0; c < dim.columns; ++c)
				{
					if (c != 0)
					{
						g_string_append (client->priv->outbuf, ", ");
					}

					g_ascii_dtostr (numbuf,
					                G_ASCII_DTOSTR_BUF_SIZE,
					                vals[i]);

					g_string_append (client->priv->outbuf, numbuf);

					++i;
				}
			}

			g_string_append_c (client->priv->outbuf, ']');
		}

		g_string_append_c (client->priv->outbuf, '\n');
	}

	if (client->priv->outbuf->len == soff)
	{
		g_string_truncate (client->priv->outbuf, 0);
		return;
	}

	if (limit > 0 && client->priv->outbuf->len >= limit)
	{
		g_socket_send_to (client->priv->socket,
		                  client->priv->address,
		                  client->priv->outbuf->str,
		                  lastlen,
		                  NULL,
		                  NULL);

		g_string_erase (client->priv->outbuf, soff, lastlen - 1);

		if (client->priv->outbuf->len > limit)
		{
			g_string_truncate (client->priv->outbuf, soff);
		}
	}
	else if (lastlen > 0)
	{
		client->priv->outbuf->str[lastlen - 1] = ' ';
	}

	if (client->priv->outbuf->len > soff)
	{
		g_socket_send_to (client->priv->socket,
		                  client->priv->address,
		                  client->priv->outbuf->str,
		                  client->priv->outbuf->len,
		                  NULL,
		                  NULL);
	}

	g_string_truncate (client->priv->outbuf, 0);
}

static gint
calculate_next_limit (CdnClient *client,
                      GPtrArray *vars,
                      gint       start,
                      guint      limit)
{
	gint len = 0;

	if (limit == 0)
	{
		return vars->len;
	}

	while (start < vars->len)
	{
		gint s;
		CdnDimension dim;

		CdnVariable *v = g_ptr_array_index (vars, start);

		cdn_variable_get_values (v, &dim);

		s = sizeof (guint16) + cdn_dimension_size (&dim) * sizeof (guint64);

		if (len + s > limit)
		{
			break;
		}

		++start;
		len += s;
	}

	return start;
}

static void
send_out_limit_binary (CdnClient *client,
                       guint      limit)
{
	GDataOutputStream *s;
	gint start;
	gint end;
	GPtrArray *vars;
	GMemoryOutputStream *mems;

	s = g_data_output_stream_new (G_OUTPUT_STREAM (client->priv->boutbuf));
	g_filter_output_stream_set_close_base_stream (G_FILTER_OUTPUT_STREAM (s),
	                                              FALSE);

	vars = client->priv->out_variables;

	mems = G_MEMORY_OUTPUT_STREAM (client->priv->boutbuf);

	start = 0;
	end = start;

	while (start < vars->len)
	{
		gint i;
		end = calculate_next_limit (client, vars, start, limit);

		if (end == start)
		{
			// Skip, whole thing is too long
			++start;
			continue;
		}

		g_data_output_stream_put_byte (s, 'x', NULL, NULL);

		// number of output
		g_data_output_stream_put_uint32 (s, end - start, NULL, NULL);

		for (i = start; i < end; ++i)
		{
			CdnVariable *v = g_ptr_array_index (vars, i);
			gdouble const *values;
			CdnDimension dim;
			gint num;
			gint j;

			values = cdn_variable_get_values (v, &dim);
			num = cdn_dimension_size (&dim);

			g_data_output_stream_put_uint16 (s, i, NULL, NULL);

			for (j = 0; j < num; ++j)
			{
				union
				{
					guint64 val;
					gdouble dval;
				} val;

				val.dval = values[j];

				g_data_output_stream_put_uint64 (s, val.val, NULL, NULL);
				g_output_stream_flush (G_OUTPUT_STREAM (s), NULL, NULL);

			}
		}

		g_output_stream_flush (G_OUTPUT_STREAM (s), NULL, NULL);

		g_socket_send_to (client->priv->socket,
		                  client->priv->address,
		                  g_memory_output_stream_get_data (mems),
		                  g_memory_output_stream_get_data_size (mems),
		                  NULL,
		                  NULL);

		g_seekable_seek (G_SEEKABLE (client->priv->boutbuf),
		                 0,
		                 G_SEEK_SET,
		                 NULL,
		                 NULL);

		start = end;
	}

	g_object_unref (s);
}

static void
send_out_limit (CdnClient *client,
                guint      limit)
{
	if (!client->priv->binary_mode)
	{
		send_out_limit_ascii (client, limit);
	}
	else
	{
		send_out_limit_binary (client, limit);
	}
}

static void
send_out (CdnClient *client)
{
	if (!(client->priv->io_mode & CDN_IO_MODE_OUTPUT) ||
	    !client->priv->out_variables)
	{
		return;
	}

	if (client->priv->throttle > 0 &&
	    client->priv->throttle_timer &&
	    g_timer_elapsed (client->priv->throttle_timer, NULL) <
	    client->priv->throttle)
	{
		return;
	}

	if (client->priv->isdatagram)
	{
		send_out_limit (client, 512);
	}
	else
	{
		send_out_limit (client, 0);
	}

	if (client->priv->throttle > 0 &&
	    !client->priv->throttle_timer)
	{
		client->priv->throttle_timer = g_timer_new ();
	}
	else if (client->priv->throttle_timer)
	{
		g_timer_reset (client->priv->throttle_timer);
	}
}

static void
send_header (CdnClient *client,
             GPtrArray *vars,
             gchar      head)
{
	gint i;

	g_string_append_c (client->priv->outbuf, head);

	for (i = 0; i < vars->len; ++i)
	{
		CdnVariable *v;
		CdnDimension dim;

		v = g_ptr_array_index (vars, i);

		g_string_append_c (client->priv->outbuf, ' ');

		cdn_expression_get_dimension (cdn_variable_get_expression (v),
		                              &dim);

		g_string_append_printf (client->priv->outbuf,
		                        "%d %d %s",
		                        dim.rows,
		                        dim.columns,
		                        cdn_variable_get_name (v));
	}

	g_string_append_c (client->priv->outbuf, '\n');

	g_socket_send_to (client->priv->socket,
	                  client->priv->address,
	                  client->priv->outbuf->str,
	                  client->priv->outbuf->len,
	                  NULL,
	                  NULL);

	g_string_truncate (client->priv->outbuf, 0);
}

static void
send_binary_header (CdnClient *client)
{
	g_socket_send_to (client->priv->socket,
	                  client->priv->address,
	                  "b\n",
	                  2,
	                  NULL,
	                  NULL);
}

static void
send_input_header (CdnClient *client)
{
	send_header (client,
	             client->priv->in_variables,
	             'i');
}

static void
send_output_header (CdnClient *client)
{
	send_header (client,
	             client->priv->out_variables,
	             'o');
}

void
cdn_client_initialize (CdnClient *client)
{
	if (client->priv->isdatagram)
	{
		return;
	}

	send_input_header (client);
	send_output_header (client);
	send_binary_header (client);
}

static gint
find_in_array (gchar const *name,
               GPtrArray   *vars)
{
	gint i;

	for (i = 0; i < vars->len; ++i)
	{
		gchar const *nm;

		nm = cdn_variable_get_name (g_ptr_array_index (vars, i));

		if (g_strcmp0 (name, nm) == 0)
		{
			return i;
		}
	}

	return -1;
}

static void
update_header_real (CdnClient      *client,
                    MessageHeader  *message,
                    GPtrArray      *vars,
                    gint          **map,
                    gint           *num_map)
{
	GSList *item;
	gint i = 0;

	g_free (*map);

	*map = NULL;
	*num_map = 0;

	if (!message->variables)
	{
		return;
	}

	*num_map = g_slist_length (message->variables);
	*map = g_new0 (gint, *num_map);

	for (item = message->variables; item; item = g_slist_next (item))
	{
		HeaderVariable *v = item->data;
		gint idx;

		idx = find_in_array (v->name, vars);

		(*map)[i] = idx;
		++i;
	}
}

static void
update_header (CdnClient     *client,
               MessageHeader *header)
{
	if (header->direction == HEADER_DIRECTION_IN)
	{
		update_header_real (client,
		                    header,
		                    client->priv->out_variables,
		                    &client->priv->output_map,
		                    &client->priv->num_output_map);
	}
	else
	{
		update_header_real (client,
		                    header,
		                    client->priv->in_variables,
		                    &client->priv->input_map,
		                    &client->priv->num_input_map);
	}
}

static CdnExpression *
value_from_expression (CdnClient   *client,
                       gchar const *expression)
{
	CdnExpression *expr;

	if (!client->priv->cctx)
	{
		client->priv->cctx =
			cdn_object_get_compile_context (CDN_OBJECT (client->priv->node),
			                                NULL);
	}

	expr = cdn_expression_new (expression);

	if (!cdn_expression_compile (expr, client->priv->cctx, NULL))
	{
		g_object_unref (expr);
		return NULL;
	}

	return expr;
}

static void
update_set (CdnClient  *client,
            MessageSet *message)
{
	CdnVariable *v = NULL;
	CdnExpression *e = NULL;
	gdouble const *value = NULL;
	CdnDimension dim = CDN_DIMENSION (0, 0);
	CdnDimension odim;

	switch (message->variable_type)
	{
		case VARIABLE_TYPE_VARIABLE:
			v = message->variable;
		break;
		case VARIABLE_TYPE_SELECTOR:
			v = cdn_node_find_variable (client->priv->node,
			                            message->selector);
		break;
	}

	if (v == NULL)
	{
		return;
	}

	switch (message->value_type)
	{
		case VALUE_TYPE_VALUE:
			value = message->value.value;
			dim.rows = message->value.numr;
			dim.columns = message->value.numc;
		break;
		case VALUE_TYPE_EXPRESSION:
		{
			e = value_from_expression (client,
			                           message->expression);

			if (e)
			{
				value = cdn_expression_evaluate_values (e,
				                                        &dim);
			}
		}
		break;
	}

	cdn_expression_get_dimension (cdn_variable_get_expression (v),
	                              &odim);

	if (value && cdn_dimension_equal (&dim, &odim))
	{
		cdn_variable_set_values (v,
		                         value,
		                         &dim);
	}

	if (e)
	{
		g_object_unref (e);
	}
}

static void
update_binary_mode (CdnClient *client)
{
	gchar *buffer;

	if (client->priv->binary_mode)
	{
		return;
	}

	buffer = g_new0 (gchar, 1024);

	client->priv->boutbuf = g_memory_output_stream_new (buffer,
	                                                    1024,
	                                                    g_realloc,
	                                                    g_free);

	client->priv->binary_mode = TRUE;
}

void
cdn_client_update (CdnClient *client)
{
	g_mutex_lock (client->priv->message_mutex);

	client->priv->messages = g_slist_reverse (client->priv->messages);

	while (client->priv->messages)
	{
		Message *msg = client->priv->messages->data;

		switch (msg->type)
		{
			case MESSAGE_TYPE_HEADER:
				update_header (client, (MessageHeader *)msg);
			break;
			case MESSAGE_TYPE_SET:
				update_set (client, (MessageSet *)msg);
			break;
			case MESSAGE_TYPE_BINARY_MODE:
				update_binary_mode (client);
			break;
		}

		message_free (msg);

		client->priv->messages =
			g_slist_delete_link (client->priv->messages,
			                     client->priv->messages);
	}

	g_mutex_unlock (client->priv->message_mutex);

	if (g_socket_is_closed (client->priv->socket))
	{
		CdnNetworkThread *t = cdn_network_thread_get_default ();

		cdn_network_thread_unregister (t, client->priv->socket);
		g_signal_emit (client, signals[CLOSED], 0);
		return;
	}

	send_out (client);
}

void
cdn_client_close (CdnClient *client)
{
	g_return_if_fail (CDN_IS_CLIENT (client));

	if (!g_socket_is_closed (client->priv->socket))
	{
		g_socket_close (client->priv->socket, NULL);
	}
}

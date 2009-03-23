#include <stdlib.h>
#include <string.h>

#include "cpg-shared-network.h"
#include "cpg-network.h"
#include "cpg-expression.h"
#include "cpg-debug.h"

#include "shared/cpg-shared-link.h"
#include "shared/cpg-shared-expression.h"

static void
memory_map_add(CpgMemoryMap *iter, void *source, CpgSharedPointer target)
{
	while (iter->next)
		iter = iter->next;

	iter->next = (CpgMemoryMap *)malloc(sizeof(CpgMemoryMap));
	iter->next->source = source;
	iter->next->target = target;
	iter->next->next = NULL;
}

static void
memory_map_free(CpgMemoryMap *iter)
{
	if (iter)
	{
		memory_map_free(iter->next);
		free(iter);
	}
}

CpgSharedPointer
cpg_memory_map_find(CpgMemoryMap *iter, void *source)
{
	if (!iter)
		return 0;

	if (iter->source == source)
		return iter->target;

	return cpg_memory_map_find(iter->next, source);
}

void
cpg_memory_map_destroy(CpgMemoryMap *map)
{
	memory_map_free(map->next);
}

void 
cpg_memory_map_init(CpgMemoryMap *map)
{
	map->target = 0;
	map->source = NULL;
	map->next = NULL;
}

void *
do_memory_add(void *x, unsigned num)
{
        return (void *)((unsigned)x + num);
}

#define memory_add(x, num, maxsize) (x = do_memory_add(x, num))
#define memory_offset(base, current) ((unsigned)current - (unsigned)base)

static int
copy_expression(CpgMemoryMap *map, void *base, void **ptr, CpgSharedExpression *expr, CpgExpression *expression, unsigned maxsize)
{
	CpgInstruction *inst;
	expr->num_instructions = 0;
	
	if (expression->instructions)
		expr->instructions = memory_offset(base, *ptr);
	else
		expr->instructions = 0;

	for (inst = expression->instructions; inst; inst = inst->next)
	{
		CpgSharedInstruction *si = (CpgSharedInstruction *)*ptr;
		++expr->num_instructions;

		memory_add(*ptr, sizeof(CpgSharedInstruction), maxsize);
		
		si->type = inst->type;
		
		switch (inst->type)
		{
			case CPG_INSTRUCTION_TYPE_FUNCTION:
			case CPG_INSTRUCTION_TYPE_OPERATOR:
				si->id = ((CpgInstructionFunction *)inst)->id;
			break;
			case CPG_INSTRUCTION_TYPE_NUMBER:
				si->value = ((CpgInstructionNumber *)inst)->value;
			break;
			case CPG_INSTRUCTION_TYPE_PROPERTY:
				si->property = cpg_memory_map_find(map, ((CpgInstructionProperty *)inst)->property);
			break;
			default:
			break;
		}
	}	
	
	// shared pointer to stack start
	expr->output.output = (double *)memory_offset(base, *ptr);
	expr->output.output_ptr = expr->output.output;

	// allocate stack
	memory_add(*ptr, sizeof(double) * expression->output.size, maxsize);
	
	return 1;
}

static int
allocate_property(CpgProperty *property, CpgMemoryMap *map, void *base, void **ptr, unsigned maxsize)
{
	CpgSharedProperty *prop = (CpgSharedProperty *)*ptr;
	memory_map_add(map, property, memory_offset(base, *ptr));

	memory_add(*ptr, sizeof(CpgSharedProperty), maxsize);
	
	prop->integrated = property->integrated;
	prop->update = 0;
	
	return 1;
}

static int
allocate_properties(CpgObject *object, CpgMemoryMap *map, void *base, void **ptr, unsigned maxsize)
{
	unsigned i;
		
	for (i = 0; i < object->num_properties; ++i)
		allocate_property(object->properties[i], map, base, ptr, maxsize);
		
	return 1;
}

static int
copy_property(CpgProperty *property, void **propstart, CpgMemoryMap *map, void *base, void **ptr, unsigned maxsize)
{
	CpgSharedProperty *prop = (CpgSharedProperty *)*propstart;
	memory_add(*propstart, sizeof(CpgSharedProperty), maxsize);
	
	// construct the expression
	copy_expression(map, base, ptr, &(prop->value), property->initial, maxsize);
	return 1;
}

static int
copy_properties(CpgObject *object, void **propstart, CpgMemoryMap *map, void *base, void **ptr, unsigned maxsize)
{
	unsigned i;

	for (i = 0; i < object->num_properties; ++i)
		copy_property(object->properties[i], propstart, map, base, ptr, maxsize);
	
	return 1;
}

/**
 * cpg_shared_network_copy:
 * @network: the #CpgNetwork to copy
 * @memory: the memory to copy the network into
 * @maxsize: the maximum amount of memory to use
 *
 * Copy network definition in compact shared memory format.
 *
 * Return value: the base address of the shared network, or NULL of the
 *	       shared network could not be constructed
 **/
void
cpg_shared_network_copy(CpgSharedNetwork *ret, CpgNetwork *network, CpgMemoryMap *map, void *base, void **ptr, unsigned maxsize)
{
	void *propstart = *ptr;
	
	// allocate properties
	ret->timeprop.integrated = 0;
	ret->timeprop.update = 0;
	ret->timestepprop.integrated = 0;
	ret->timestepprop.update = 0;
	
	memory_map_add(map, network->timeprop, memory_offset(base, &(ret->timeprop)));
	memory_map_add(map, network->timestepprop, memory_offset(base, &(ret->timeprop)));

	unsigned i;
	for (i = 0; i < network->num_states; ++i)
		allocate_properties((CpgObject *)network->states[i], map, base, ptr, maxsize);
	
	for (i = 0; i < network->num_links; ++i)
		allocate_properties((CpgObject *)network->links[i], map, base, ptr, maxsize);
	
	// fill in property expressions
	copy_expression(map, base, ptr, &(ret->timeprop.value), network->timeprop->initial, maxsize);
	copy_expression(map, base, ptr, &(ret->timestepprop.value), network->timestepprop->initial, maxsize);
	
	for (i = 0; i < network->num_states; ++i)
		copy_properties((CpgObject *)network->states[i], &propstart, map, base, ptr, maxsize);
	
	for (i = 0; i < network->num_links; ++i)
		copy_properties((CpgObject *)network->links[i], &propstart, map, base, ptr, maxsize);
	
	// fill in all the actor property references
	ret->num_actors = 0;
	ret->actors = memory_offset(base, *ptr); // shared pointer to first actor
		
	for (i = 0; i < network->num_states; ++i)
	{
		CpgObject *object = (CpgObject *)network->states[i];
		unsigned a;
		
		ret->num_actors += object->num_actors;

		for (a = 0; a < object->num_actors; ++a)
		{
			CpgSharedPointer addr = cpg_memory_map_find(map, object->actors[a]);
			memcpy(*ptr, &addr, sizeof(CpgSharedPointer));
			memory_add(*ptr, sizeof(CpgSharedPointer), maxsize);
		}
	}
	
	// create all the links
	ret->num_links = network->num_links;
	ret->links = memory_offset(base, *ptr); // shared pointer to first link
	
	CpgSharedLink *link = (CpgSharedLink *)*ptr;
	memory_add(*ptr, sizeof(CpgSharedLink) * network->num_links, maxsize);
	
	for (i = 0; i < network->num_links; ++i)
	{
		// create link
		CpgSharedLinkAction *action = (CpgSharedLinkAction *)*ptr;
		link->actions = memory_offset(base, *ptr); // shared pointer to first action

		link->num_actions = network->links[i]->num_actions;
		memory_add(*ptr, sizeof(CpgSharedLinkAction) * link->num_actions, maxsize);
		
		unsigned a;
		for (a = 0; a < network->links[i]->num_actions; ++a)
		{
			action->target = cpg_memory_map_find(map, network->links[i]->actions[a]->target);
			
			// copy expression
			copy_expression(map, base, ptr, &(action->expression), network->links[i]->actions[a]->expression, maxsize);
			++action;
		}
				
		++link;
	}
}

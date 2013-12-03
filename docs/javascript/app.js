var App = function() {
    this._init();
};

App.prototype = {
    _init: function() {
    },

    _install_cldoc_trigger: function() {
        if (typeof($) == 'undefined') {
            return;
        }

        var cldoc = $('#cldoc');

        var t = this;

        cldoc.on('page-loaded', function() {
            t._load_elems(t._find_codes());
        });
    },

    _find_codes: function() {
        return document.querySelectorAll('code[class="lang-cdn"]');
    },

    _play_click: function(event, code) {
        var f = document.createElement('form');
        f.method = 'post';
        f.action = 'http://play.codyn.net/d/';

        if (event.ctrlKey)
        {
            f.target = "_blank";
        }

        var tt = document.createElement('textarea');
        tt.value = code;
        tt.name = 'document';

        f.appendChild(tt);
        f.style.display = 'none';

        document.body.appendChild(f);
        f.submit();

        document.body.removeChild(f);
        return false;
    },

    _load_elems: function(elems) {
        for (var i = 0; i < elems.length; i++) {
            var e = elems[i];
            var code = e.innerText;
            e.innerText = '';

            var cm = CodeMirror(e, {
                value: code,
                mode: 'codyn',
                readOnly: true
            });

            var wrapper = cm.display.wrapper;
            var p = document.createElement('div');
            var a = document.createElement('a');

            a.href = '#';
            a.onclick = function (t) {
                return function(event) {
                    return t._play_click(event, code);
                };
            }(this, code);

            p.className = 'play';

            a.innerHTML = 'Open in playground';
            p.appendChild(a);

            e.appendChild(p);
        }
    },

    load: function(elems) {
        var elems = this._find_codes();

        if (elems.length == 0) {
            this._install_cldoc_trigger();
        } else {
            this._load_elems(elems);
        }
    }
};

var app = new App();

if (typeof(body) != 'undefined' && body.readyState == 'loaded') {
    app.load();
} else {
    window.addEventListener('load', function() {
        app.load();
    }, false);
}

/* vi:ts=4:et */

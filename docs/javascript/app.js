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

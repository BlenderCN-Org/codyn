var plot_data = null;
var plot_selection = null;

var colors_html = [
    "#268BD2",
    "#859900",
    "#DC322F",
    "#2AA198",
    "#D33682",
    "#B58900",
    "#6C71C4",
    "#CB4B16"
];

function show_plot_tooltip(x, y, contents) {
    var tp = $('#plot_tooltip');
    var cb;

    if (tp.length == 0) {
        tp = $('<div id="plot_tooltip"/>');

        cb = function(e) {
            e.fadeIn(200);
        };
    } else {
        cb = function(e) {
            e.show();
        };
    }

    tp.text(contents);

    tp.css({
        position: 'absolute',
        display: 'none',
        top: y + 5,
        left: x + 5
    }).appendTo("body");

    cb(tp);
}

function update_plot() {
	var series = [];
    var i = 0;

    for (var i = 0; i < plot_data.lines.length; i++)
    {
        var line = plot_data.lines[i];
        var yval = line.data;
        var d = [];

        for (var j = 0; j < yval.length; ++j) {
            d.push([plot_data.t[j], yval[j]]);
        }

        var serie = {
            label: line.label,
            data: d,
            color: colors_html[i % colors_html.length]
        };

        series.push(serie);
    }

    // series.sort(function(a, b) {
    //     return a.label < b.label ? -1 : (a.label > b.label ? 1 : 0);
    // });

    var options = {
        grid: {
            hoverable: true
        },

        selection: {
            mode: 'xy'
        }
    };

    if (plot_selection != null) {
        options.xaxis = { min: plot_selection.xaxis.from, max: plot_selection.xaxis.to };
        options.yaxis = { min: plot_selection.yaxis.from, max: plot_selection.yaxis.to };
    }

    $.plot('#plot', series, options);
}

function get_data() {
	$.get('/data', {}, function(data, textStatus) {
		setTimeout(get_data, 0);

        plot_data = data;
		update_plot();
	}, 'json');
}

$(document).ready(function() {
	$('#plot').bind('plotselected', function (event, ranges) {
        plot_selection = ranges;
        update_plot();
    });

    $('#plot').bind('plotunselected', function (event) {
        plot_selection = null;
        update_plot();
    });

    $('#plot').bind('plothover', function(ev, pos, item) {
    if (!item) {
        $('#plot_tooltip').remove();
        previous_point = null;
        return;
    }

    var pt = [item.seriesIndex, item.dataIndex];

    if (previous_point != pt) {
        previous_point = pt;

        var x = item.datapoint[0].toFixed(2);
        var y = item.datapoint[1].toFixed(5);

        show_plot_tooltip(item.pageX, item.pageY, item.series.label + " at " + x + " = " + y);
    }
});

    get_data();
})
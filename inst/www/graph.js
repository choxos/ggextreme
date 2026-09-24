// Behavior shared by the interactive graphs: sizing, the dark theme, the
// panel that opens on click, and the collapsed sections under a plot.
//
// The panel sits directly after the widget, one per widget, and is created
// on the first click. Clicking the same item again, or the close button,
// hides it; clicking another item replaces its contents.
(function () {
  // Whether the page around a widget is dark. A widget saved as a page of
  // its own follows the viewer's system setting.
  function pageIsDark() {
    var html = document.documentElement;
    var body = document.body;
    var marked = html.getAttribute('data-bs-theme') || html.getAttribute('data-theme') ||
      (body && (body.getAttribute('data-bs-theme') || body.getAttribute('data-theme')));
    var system = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
    if (marked) return marked === 'auto' ? system : marked === 'dark';
    if (body && body.classList.contains('quarto-dark')) return true;
    if (body && body.classList.contains('quarto-light')) return false;
    if (document.getElementById('htmlwidget_container')) return system;
    return false;
  }

  // Everything that belongs to a widget and sits beside it in the page.
  function companions(host) {
    var out = [];
    var next = host.nextElementSibling;
    while (next && (next.classList.contains('ggx-panel') || next.classList.contains('ggx-details'))) {
      out.push(next);
      next = next.nextElementSibling;
    }
    return out;
  }

  function paint(el) {
    var dark = el.getAttribute('data-ggx-theme') === 'dark' ||
      (el.getAttribute('data-ggx-theme') === 'auto' && pageIsDark());
    el.classList.toggle('ggx-dark', dark);
    companions(el).forEach(function (c) { c.classList.toggle('ggx-dark', dark); });
    // Hover cards are added to the page body, so the body carries their
    // theme. toggle() with a state leaves the attribute alone when nothing
    // changes; add() would rewrite it, and the observer watching the body
    // would repaint forever.
    document.body.classList.toggle('ggx-dark-tips',
                                   dark || !!document.querySelector('.ggx-graph.ggx-dark'));
    if (document.getElementById('htmlwidget_container')) {
      document.body.style.backgroundColor = dark ? '#161C1C' : '';
    }
  }

  function followPage(el) {
    var redraw = function () { paint(el); };
    if (window.MutationObserver) {
      var watch = new MutationObserver(redraw);
      var options = { attributes: true, attributeFilter: ['data-bs-theme', 'data-theme', 'class'] };
      watch.observe(document.documentElement, options);
      if (document.body) watch.observe(document.body, options);
    }
    if (window.matchMedia) {
      var query = window.matchMedia('(prefers-color-scheme: dark)');
      if (query.addEventListener) query.addEventListener('change', redraw);
    }
  }

  function hostOf(el) {
    return el.closest('.html-widget') || el.closest('svg').parentNode;
  }

  // A widget with a collapsed section keeps its panel inside, just above the
  // section, so a click's readout lands next to the plot.
  function panelFor(host) {
    var section = host.querySelector(':scope > .ggx-details');
    var next = section ? section.previousElementSibling : host.nextElementSibling;
    if (next && next.classList.contains('ggx-panel')) return next;
    var panel = document.createElement('div');
    panel.className = 'ggx-panel' + (host.classList.contains('ggx-dark') ? ' ggx-dark' : '');
    panel.setAttribute('role', 'region');
    panel.setAttribute('aria-live', 'polite');
    panel.hidden = true;
    if (section) host.insertBefore(panel, section);
    else host.parentNode.insertBefore(panel, host.nextSibling);
    return panel;
  }

  // Cap the widget at the diagram's natural width, given by its viewBox in
  // points, let its height follow the width, and set its theme: "auto"
  // follows the page, "light" and "dark" fix it.
  window.ggextremeFit = function (el, theme) {
    el.classList.add('ggx-graph');
    el.setAttribute('data-ggx-theme', theme || 'auto');
    var svg = el.querySelector('svg');
    if (svg && svg.viewBox && svg.viewBox.baseVal) {
      el.style.maxWidth = (svg.viewBox.baseVal.width * 4 / 3) + 'px';
      el.style.height = 'auto';
    }
    paint(el);
    if ((theme || 'auto') === 'auto') followPage(el);
  };

  // A section under the plot, collapsed until the reader opens it. It goes
  // inside the widget, so it keeps the plot's width and position and takes
  // its theme.
  window.ggextremeDetails = function (el, title, html) {
    var box = document.createElement('details');
    box.className = 'ggx-details';
    var summary = document.createElement('summary');
    summary.textContent = title;
    var body = document.createElement('div');
    body.className = 'ggx-details-body';
    body.innerHTML = html;
    box.appendChild(summary);
    box.appendChild(body);
    el.appendChild(box);
  };

  // In a league table, hovering a treatment (id t<p>, on the diagonal or in
  // the ranking) lights the cells of its row and column (ids c<row>_<col>).
  window.ggextremeLeague = function (el) {
    var svg = el.querySelector('svg');
    if (!svg) return;
    var cells = [].slice.call(svg.querySelectorAll('[data-id^="c"]'));
    function treatmentOf(target) {
      var id = target && target.getAttribute && target.getAttribute('data-id');
      var m = id && /^t(\d+)$/.exec(id);
      return m ? m[1] : null;
    }
    svg.addEventListener('pointerover', function (ev) {
      var p = treatmentOf(ev.target);
      if (p === null) return;
      cells.forEach(function (c) {
        var q = /^c(\d+)_(\d+)$/.exec(c.getAttribute('data-id'));
        if (q) c.style.opacity = (q[1] === p || q[2] === p) ? '1' : '0.25';
      });
    });
    svg.addEventListener('pointerout', function (ev) {
      if (treatmentOf(ev.target) === null) return;
      cells.forEach(function (c) { c.style.opacity = ''; });
    });
  };

  // In a Kaplan-Meier plot, hovering a time slice (id t<slice>_<column>)
  // lights the matching column of the risk table (ids r<column>_<arm>).
  window.ggextremeKm = function (el) {
    var svg = el.querySelector('svg');
    if (!svg) return;
    var cells = [].slice.call(svg.querySelectorAll('[data-id^="r"]'));
    function columnOf(target) {
      var id = target && target.getAttribute && target.getAttribute('data-id');
      var m = id && /^t\d+_(\d+)$/.exec(id);
      return m ? m[1] : null;
    }
    svg.addEventListener('pointerover', function (ev) {
      var col = columnOf(ev.target);
      if (col === null) return;
      cells.forEach(function (c) {
        var q = /^r(\d+)_\d+$/.exec(c.getAttribute('data-id'));
        if (q) c.style.opacity = q[1] === col ? '1' : '0.3';
      });
    });
    svg.addEventListener('pointerout', function (ev) {
      if (columnOf(ev.target) === null) return;
      cells.forEach(function (c) { c.style.opacity = ''; });
    });
  };

  function escapeHtml(s) {
    return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;')
      .replace(/>/g, '&gt;').replace(/"/g, '&quot;');
  }

  // A row of controls under the plot, above any collapsed section.
  function addControls(el, html) {
    var bar = document.createElement('div');
    bar.className = 'ggx-controls';
    bar.innerHTML = html;
    var section = el.querySelector(':scope > .ggx-details');
    if (section) el.insertBefore(bar, section);
    else el.appendChild(bar);
    return bar;
  }

  // A funnel plot with trim and fill: a switch shows or hides the imputed
  // studies and the adjusted estimate (ids starting tf).
  window.ggextremeFunnel = function (el) {
    var svg = el.querySelector('svg');
    if (!svg) return;
    var filled = [].slice.call(svg.querySelectorAll('[data-id^="tf"]'));
    if (!filled.length) return;
    var bar = addControls(el, '<label class="ggx-check"><input type="checkbox" checked> ' +
      'Show trim and fill</label>');
    bar.querySelector('input').addEventListener('change', function (ev) {
      filled.forEach(function (node) { node.style.display = ev.target.checked ? '' : 'none'; });
    });
  };

  // A swimmer plot. Every mark in a lane carries the lane's id (p<lane>);
  // the buttons move each lane to its row in the chosen order.
  window.ggextremeSwimmer = function (el, data) {
    var svg = el.querySelector('svg');
    if (!svg || !data) return;
    el.classList.add('ggx-swimmer');
    var lanes = [];
    [].forEach.call(svg.querySelectorAll('[data-id^="p"]'), function (node) {
      var m = /^p(\d+)$/.exec(node.getAttribute('data-id'));
      if (m) (lanes[m[1] - 1] = lanes[m[1] - 1] || []).push(node);
    });
    var first = data.orders.filter(function (o) { return o.key === data.start; })[0];
    function place(order) {
      lanes.forEach(function (nodes, i) {
        var dy = (order.row[i] - first.row[i]) * data.row_h;
        nodes.forEach(function (node) {
          node.style.transform = dy ? 'translate(0px, ' + dy + 'px)' : '';
        });
      });
    }
    var bar = addControls(el, '<span>Order</span><div class="ggx-seg" role="group" ' +
      'aria-label="Order of the lanes">' + data.orders.map(function (o) {
        return '<button type="button" data-key="' + escapeHtml(o.key) + '" aria-pressed="' +
          (o.key === data.start) + '">' + escapeHtml(o.label) + '</button>';
      }).join('') + '</div>');
    var buttons = [].slice.call(bar.querySelectorAll('button'));
    buttons.forEach(function (b, k) {
      b.addEventListener('click', function () {
        buttons.forEach(function (o) { o.setAttribute('aria-pressed', o === b); });
        place(data.orders[k]);
      });
    });
  };

  var icons = {
    play: '<svg viewBox="0 0 12 12" aria-hidden="true"><path d="M2.5 1.2l8 4.8-8 4.8z"/></svg>',
    pause: '<svg viewBox="0 0 12 12" aria-hidden="true"><path d="M2 1h3v10H2zM7 1h3v10H7z"/></svg>'
  };

  // A choropleth. Every region is a path with id g<region> on each map, and
  // each map is its own layer, so a region's paths come in map order. The
  // slider and play button choose the time; each path then takes its fill
  // for that time from `data`, and its hover card is rewritten.
  window.ggextremeMap = function (el, data) {
    var svg = el.querySelector('svg');
    if (!svg || !data) return;
    el.classList.add('ggx-map');
    var panels = data.panels;
    var times = data.times;
    var last = times.length - 1;
    var shapes = [];
    [].forEach.call(svg.querySelectorAll('[data-id^="g"]'), function (p) {
      var r = regionOf(p);
      if (r !== null) (shapes[r] = shapes[r] || []).push(p);
    });
    var stamps = [].slice.call(svg.querySelectorAll('[data-id="yr"]'));
    var has = data.names.map(function (_, r) {
      return panels.some(function (p) {
        return p.value.some(function (row) { return row[r] !== null; });
      });
    });
    var current = data.start;
    var hovered = null;
    var timer = null;
    var slider = null;
    var button = null;

    function regionOf(node) {
      var id = node && node.getAttribute && node.getAttribute('data-id');
      var m = id && /^g(\d+)$/.exec(id);
      return m ? parseInt(m[1], 10) - 1 : null;
    }

    function number(p, v) {
      if (v === null || v === undefined) return 'No data';
      return v.toLocaleString('en-US', {
        minimumFractionDigits: p.digits, maximumFractionDigits: p.digits,
        useGrouping: !!p.grouping
      });
    }

    // Rank 1 is the highest value at that time; ties share a rank.
    function ranks(row) {
      var order = [];
      row.forEach(function (v, r) { if (v !== null) order.push(r); });
      order.sort(function (a, b) { return row[b] - row[a]; });
      var at = {};
      order.forEach(function (r, i) {
        at[r] = i > 0 && row[order[i - 1]] === row[r] ? at[order[i - 1]] : i + 1;
      });
      return { at: at, n: order.length };
    }

    function tip(r, t, rk) {
      var rows = panels.map(function (p, k) {
        var v = p.value[t][r];
        return '<div class="ggx-tip-row"><span>' + escapeHtml(p.label) + '</span><span>' +
          number(p, v) + (v === null ? '' : ' <span class="ggx-rank">#' + rk[k].at[r] +
          ' of ' + rk[k].n + '</span>') + '</span></div>';
      }).join('');
      return '<div class="ggx-tip-title">' + escapeHtml(data.names[r]) + '</div>' +
        '<div class="ggx-tip-sub">' + escapeHtml(times[t]) + '</div>' + rows +
        (has[r] ? '<div class="ggx-tip-hint">Click for the whole series.</div>' : '');
    }

    function show(t) {
      current = t;
      var rk = panels.map(function (p) { return ranks(p.value[t]); });
      var box = hovered === null ? null : document.querySelector('div.tooltip_' + svg.id);
      shapes.forEach(function (paths, r) {
        var html = tip(r, t, rk);
        // ggiraph decodes the title once before showing it.
        var title = html.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
        paths.forEach(function (path, k) {
          var p = panels[k];
          if (!p) return;
          path.setAttribute('fill', p.tones[p.fill[t][r]]);
          path.setAttribute('fill-opacity', p.alpha[t][r]);
          path.setAttribute('title', title);
        });
        // The card showing now is refreshed, since ggiraph only reads the
        // title when the pointer enters a region.
        if (box && r === hovered) box.innerHTML = html;
      });
      stamps.forEach(function (s) { s.textContent = times[t]; });
      if (slider) {
        slider.value = t;
        slider.setAttribute('aria-valuetext', times[t]);
      }
    }

    function spark(ys, p) {
      var W = 360, H = 78, L = 46, R = 12, T = 8, B = 22;
      var seen = ys.filter(function (v) { return v !== null; });
      if (!seen.length) return '<p class="ggx-sub">No data.</p>';
      var min = Math.min.apply(null, seen);
      var max = Math.max.apply(null, seen);
      var lo = min, hi = max;
      if (lo === hi) { lo -= 1; hi += 1; }
      function x(i) { return (L + (last ? i / last : 0.5) * (W - L - R)).toFixed(1); }
      function y(v) { return (T + (hi - v) / (hi - lo) * (H - T - B)).toFixed(1); }
      function text(s, tx, ty, anchor) {
        return '<text x="' + tx + '" y="' + ty + '" text-anchor="' + anchor + '">' +
          escapeHtml(s) + '</text>';
      }
      var runs = [], run = [];
      ys.forEach(function (v, i) {
        if (v === null) { if (run.length) runs.push(run); run = []; }
        else run.push([x(i), y(v)]);
      });
      if (run.length) runs.push(run);
      var marks = runs.map(function (pts) {
        if (pts.length === 1) {
          return '<circle cx="' + pts[0][0] + '" cy="' + pts[0][1] + '" r="2" fill="' + p.line + '"/>';
        }
        return '<polyline points="' + pts.map(function (q) { return q.join(','); }).join(' ') +
          '" fill="none" stroke="' + p.line + '" stroke-width="2" stroke-linejoin="round"/>';
      }).join('');
      var now = ys[current] === null ? '' : '<circle cx="' + x(current) + '" cy="' + y(ys[current]) +
        '" r="3.5" fill="' + p.line + '" class="ggx-spark-now"/>';
      var first = ys.findIndex(function (v) { return v !== null; });
      var end = ys.length - 1 - ys.slice().reverse().findIndex(function (v) { return v !== null; });
      return '<svg class="ggx-spark" viewBox="0 0 ' + W + ' ' + H + '" role="img" aria-label="' +
        escapeHtml(p.label + ', ' + times[0] + ' to ' + times[last]) + '">' +
        '<line x1="' + L + '" y1="' + (H - B) + '" x2="' + (W - R) + '" y2="' + (H - B) + '"/>' +
        marks + now +
        text(number(p, max), L - 7, Number(y(max)) + 4, 'end') +
        (max === min ? '' : text(number(p, min), L - 7, Number(y(min)) + 4, 'end')) +
        text(times[0], L, H - 6, 'start') + text(times[last], W - R, H - 6, 'end') + '</svg>' +
        '<p class="ggx-sub">' + number(p, ys[first]) + ' in ' + escapeHtml(times[first]) +
        (end > first ? ', ' + number(p, ys[end]) + ' in ' + escapeHtml(times[end]) : '') +
        (ys[current] === null ? '' : '. The dot marks ' + escapeHtml(times[current])) + '.</p>';
    }

    function series(r) {
      return '<div class="ggx-title">' + escapeHtml(data.names[r]) + '</div>' +
        '<div class="ggx-sub">' + escapeHtml(times[0]) + ' to ' + escapeHtml(times[last]) + '</div>' +
        '<div class="ggx-series">' + panels.map(function (p) {
          return '<div><div class="ggx-refs-head">' + escapeHtml(p.label) + '</div>' +
            spark(p.value.map(function (row) { return row[r]; }), p) + '</div>';
        }).join('') + '</div>';
    }

    svg.addEventListener('pointerover', function (ev) {
      var r = regionOf(ev.target);
      if (r === null) return;
      hovered = r;
      // Bring the region forward so its outline is not hidden by neighbors.
      shapes[r].forEach(function (p) { if (p.nextElementSibling) p.parentNode.appendChild(p); });
    });
    svg.addEventListener('pointerout', function (ev) {
      if (regionOf(ev.target) === hovered) hovered = null;
    });
    svg.addEventListener('click', function (ev) {
      var r = regionOf(ev.target);
      if (r !== null && has[r]) window.ggextremePin(ev.target, 'g' + r, series(r));
    });
    stamps.forEach(function (s) { s.style.pointerEvents = 'none'; });

    function stop() {
      if (timer) clearInterval(timer);
      timer = null;
      if (button) {
        button.innerHTML = icons.play;
        button.setAttribute('aria-label', 'Play');
      }
    }
    function play() {
      if (current >= last) show(0);
      button.innerHTML = icons.pause;
      button.setAttribute('aria-label', 'Pause');
      timer = setInterval(function () {
        if (current >= last) stop();
        else show(current + 1);
      }, data.interval);
    }

    if (last > 0) {
      var bar = addControls(el, '<button type="button" class="ggx-play" aria-label="Play">' +
        icons.play + '</button><span>' + escapeHtml(times[0]) + '</span>' +
        '<input type="range" min="0" max="' + last + '" step="1" value="' + current +
        '" aria-label="Time"><span>' + escapeHtml(times[last]) + '</span>');
      button = bar.querySelector('button');
      slider = bar.querySelector('input');
      button.addEventListener('click', function () { if (timer) stop(); else play(); });
      slider.addEventListener('input', function () {
        stop();
        show(parseInt(slider.value, 10));
      });
    }
    show(current);
  };

  window.ggextremePin = function (el, key, html) {
    var host = hostOf(el);
    var panel = panelFor(host);
    if (panel.parentNode !== host) {
      // Beside the widget, a panel with a table gets room for it even under
      // a narrow plot, and lines up with the widget.
      var box = window.getComputedStyle(host);
      var cap = parseFloat(host.style.maxWidth) || 0;
      panel.style.maxWidth = Math.max(cap, 760) + 'px';
      panel.style.marginLeft = box.marginLeft;
      panel.style.marginRight = box.marginRight;
    }
    if (!panel.hidden && panel.getAttribute('data-key') === key) {
      panel.hidden = true;
      return;
    }
    panel.setAttribute('data-key', key);
    panel.innerHTML =
      '<button type="button" class="ggx-close" aria-label="Close">\u00d7</button>' +
      html;
    panel.querySelector('.ggx-close').addEventListener('click', function () {
      panel.hidden = true;
    });
    panel.hidden = false;
  };
})();

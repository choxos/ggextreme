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
  window.ggextremeLeague = function (el, data) {
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
    if (data && data.flow) leagueFlow(el, svg, data.flow);
  };

  // Where a league table's network estimate comes from: hovering or tapping
  // one of a pair's cells outlines the direct cells it draws on, each with
  // its share, and says so under the table. A tap keeps the marks.
  function leagueFlow(el, svg, flow) {
    var NS = 'http://www.w3.org/2000/svg';
    var byCell = {};
    flow.forEach(function (f) { f.cells.forEach(function (c) { byCell[c] = f; }); });
    var layer = document.createElementNS(NS, 'g');
    layer.setAttribute('class', 'ggx-flow');
    svg.appendChild(layer);
    var readout = document.createElement('div');
    readout.className = 'ggx-readout';
    readout.setAttribute('aria-live', 'polite');
    var section = el.querySelector(':scope > .ggx-details');
    if (section) el.insertBefore(readout, section); else el.appendChild(readout);
    var hint = 'Hover over or tap any estimate to mark, above the diagonal, the direct comparisons its network estimate draws on and the share of it each one carries.';
    var kept = null;
    function pct(v) { return (100 * v).toFixed(v < 0.01 ? 1 : 0) + '%'; }
    function box(id) {
      var shape = svg.querySelector('polygon[data-id="' + id + '"], path[data-id="' + id + '"]');
      return shape ? shape.getBBox() : null;
    }
    function show(f) {
      while (layer.firstChild) layer.removeChild(layer.firstChild);
      if (!f) {
        readout.innerHTML = '<p class="ggx-readout-note">' + hint + '</p>';
        return;
      }
      f.sources.forEach(function (s) {
        var b = box(s.cell);
        if (!b) return;
        var r = document.createElementNS(NS, 'rect');
        var w = 1.5 + 4 * s.share;
        r.setAttribute('x', b.x + w / 2); r.setAttribute('y', b.y + w / 2);
        r.setAttribute('width', Math.max(0, b.width - w)); r.setAttribute('height', Math.max(0, b.height - w));
        r.setAttribute('rx', 4);
        r.setAttribute('class', 'ggx-flow-box');
        r.setAttribute('stroke-width', w);
        layer.appendChild(r);
        var t = document.createElementNS(NS, 'text');
        t.setAttribute('x', b.x + b.width - 4); t.setAttribute('y', b.y + 11);
        t.setAttribute('text-anchor', 'end');
        t.setAttribute('class', 'ggx-flow-pct');
        t.textContent = pct(s.share);
        layer.appendChild(t);
      });
      var top = f.sources.slice(0, 4).map(function (s) {
        return escapeHtml(s.label) + ' <b>' + pct(s.share) + '</b>';
      }).join(', ');
      readout.innerHTML = '<p><b>' + escapeHtml(f.label) + '</b>: ' +
        (f.direct > 0 ? pct(f.direct) + ' of the network estimate comes from the trials that compare the two directly. '
          : 'no trial compares the two directly, so all of the network estimate comes through other comparisons. ') +
        'It draws on ' + top + (f.sources.length > 4 ? ' and ' + (f.sources.length - 4) + ' more' : '') + '.</p>' +
        '<p class="ggx-readout-note">' + (kept ? 'Tap the cell again, or another one, to change this. ' : '') +
        'Shares describe where the information comes from, not how trustworthy it is.</p>';
    }
    function cellOf(target) {
      var id = target && target.getAttribute && target.getAttribute('data-id');
      return id && byCell[id] ? byCell[id] : null;
    }
    svg.addEventListener('pointerover', function (ev) {
      var f = cellOf(ev.target);
      if (f) show(f);
    });
    svg.addEventListener('pointerout', function (ev) {
      if (cellOf(ev.target)) show(kept);
    });
    svg.addEventListener('click', function (ev) {
      var f = cellOf(ev.target);
      if (!f) return;
      kept = kept === f ? null : f;
      show(kept || f);
    });
    show(null);
  }

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
      if (!m) return;
      // A patient's trajectory, under the lanes, keeps its place.
      if (data.split && node.getBBox && node.getBBox().y > data.split) return;
      (lanes[m[1] - 1] = lanes[m[1] - 1] || []).push(node);
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

  // Adjustment paths in a causal diagram. Every path between the exposure
  // and the outcome is judged open or blocked for the variables adjusted
  // for, by the same rules as causal_assess() in R/causal.R. Clicking a
  // variable adjusts for it; the arrows, the boxes and the panel follow.
  window.ggextremeCausalPaths = function (el, data) {
    var svg = el.querySelector('svg');
    if (!svg || !data) return;
    var nodes = data.nodes, edges = data.edges, ink = data.ink;
    var at = {};
    nodes.forEach(function (n, i) { at[n.name] = i; });
    function label(v) { return nodes[at[v]].label; }
    function arrow(a, b) { return edges.some(function (e) { return e.from === a && e.to === b; }); }
    function neighbors(v) {
      var out = [];
      edges.forEach(function (e) {
        if (e.from === v && out.indexOf(e.to) < 0) out.push(e.to);
        if (e.to === v && out.indexOf(e.from) < 0) out.push(e.from);
      });
      return out;
    }
    var below = {};
    nodes.forEach(function (n) {
      var out = [], stack = [n.name];
      while (stack.length) {
        var u = stack.shift();
        edges.forEach(function (e) {
          if (e.from === u && out.indexOf(e.to) < 0) { out.push(e.to); stack.push(e.to); }
        });
      }
      below[n.name] = out;
    });
    function allPaths(x, y) {
      var found = [];
      (function walk(path) {
        if (found.length >= 500) return;
        var v = path[path.length - 1];
        if (v === y) { found.push(path); return; }
        neighbors(v).forEach(function (w) { if (path.indexOf(w) < 0) walk(path.concat([w])); });
      })([x]);
      return found;
    }
    function assess(x, y, adjust, paths) {
      var rows = paths.map(function (p) {
        var causal = true, open = true, why = [];
        for (var i = 1; i < p.length; i++) if (!arrow(p[i - 1], p[i])) causal = false;
        for (i = 1; i < p.length - 1; i++) {
          var m = p[i];
          if (arrow(p[i - 1], m) && arrow(p[i + 1], m)) {
            var opener = [m].concat(below[m]).filter(function (v) { return adjust.indexOf(v) >= 0; });
            if (!opener.length) {
              open = false;
              why.push(label(m) + ' is a collider, which blocks the path');
            } else if (opener[0] === m) {
              why.push('adjusting for the collider ' + label(m) + ' opens it');
            } else {
              why.push('adjusting for ' + label(opener[0]) + ', a consequence of the collider ' + label(m) + ', opens it');
            }
          } else if (adjust.indexOf(m) >= 0) {
            open = false;
            why.push('adjusting for ' + label(m) + ' blocks it');
          }
        }
        if (!open) why = why.filter(function (w) { return w.indexOf('opens it') < 0; });
        if (open && !why.length) why.push(causal ? 'the effect to estimate; nothing on it is adjusted for' : 'nothing on it is adjusted for');
        return { path: p, causal: causal, open: open, why: why.join('; ') };
      });
      // Open biasing paths first, as they are what an adjustment set must
      // fix, then causal paths, then blocked ones.
      var order = function (r) { return !r.open ? 2 : r.causal ? 1 : 0; };
      rows = rows.map(function (r, i) { return [r, i]; }).sort(function (a, b) {
        return order(a[0]) - order(b[0]) || a[1] - b[1];
      }).map(function (a) { return a[0]; });
      var mediators = adjust.filter(function (v) { return below[x].indexOf(v) >= 0; });
      var openBias = rows.filter(function (r) { return !r.causal && r.open; }).length;
      return { rows: rows, mediators: mediators, openBias: openBias, sufficient: !openBias && !mediators.length };
    }
    function minimalSets(x, y, paths) {
      var cand = nodes.filter(function (n) {
        return !n.hidden && n.name !== x && n.name !== y && below[x].indexOf(n.name) < 0;
      }).map(function (n) { return n.name; });
      if (cand.length > 12) return null;
      var out = [];
      for (var mask = 0; mask < (1 << cand.length); mask++) {
        var z = cand.filter(function (_, j) { return mask & (1 << j); });
        if (out.some(function (o) { return o.every(function (v) { return z.indexOf(v) >= 0; }); })) continue;
        if (assess(x, y, z, paths).sufficient) out.push(z);
      }
      // Smallest first, as a reader would try them.
      return out.sort(function (a, b) { return a.length - b.length; });
    }

    // The visible stroke and arrowhead of each arrow; the wide, almost
    // transparent copy on top only catches the pointer.
    var marks = edges.map(function (e) {
      return [].slice.call(svg.querySelectorAll('[data-id="' + e.id + '"]')).filter(function (m) {
        return m.getAttribute('stroke-opacity') !== '0.01';
      });
    });
    var boxes = nodes.map(function (_, i) { return svg.querySelector('[data-id="adj' + (i + 1) + '"]'); });
    // The verdict under the diagram is for a static copy; here the panel
    // gives it.
    [].slice.call(svg.querySelectorAll('[data-id="cv"]')).forEach(function (t) { t.style.display = 'none'; });

    var x = data.exposure, y = data.outcome, adjust = data.adjust.slice(), mode = 'adjust';
    var paths, result, sets, focus = null;
    var options = function (sel) {
      return nodes.map(function (n) {
        return '<option value="' + escapeHtml(n.name) + '"' + (n.name === sel ? ' selected' : '') + '>' +
          escapeHtml(n.label) + '</option>';
      }).join('');
    };
    var bar = addControls(el,
      '<label class="ggx-range">Exposure <select class="ggx-select" data-k="x">' + options(x) + '</select></label>' +
      '<label class="ggx-range">Outcome <select class="ggx-select" data-k="y">' + options(y) + '</select></label>' +
      '<span class="ggx-range">Clicking a variable <span class="ggx-seg" role="group" aria-label="What clicking a variable does">' +
      '<button type="button" data-mode="adjust" aria-pressed="true">Adjusts for it</button>' +
      '<button type="button" data-mode="read" aria-pressed="false">Opens its rationale</button></span></span>');
    var panel = document.createElement('div');
    panel.className = 'ggx-readout ggx-paths';
    el.insertBefore(panel, bar.nextSibling);
    var live = document.createElement('div');
    live.className = 'ggx-sr';
    live.setAttribute('aria-live', 'polite');
    el.insertBefore(live, panel.nextSibling);

    function stateOf(k) {
      var rank = { none: 0, blocked: 1, causal: 2, biasing: 3 }, s = 'none', e = edges[k];
      result.rows.forEach(function (r, j) {
        if (focus !== null && focus !== j) return;
        for (var i = 1; i < r.path.length; i++) {
          var a = r.path[i - 1], b = r.path[i];
          if ((e.from === a && e.to === b) || (e.from === b && e.to === a)) {
            var t = !r.open ? 'blocked' : r.causal ? 'causal' : 'biasing';
            if (rank[t] > rank[s]) s = t;
          }
        }
      });
      return s;
    }
    function paint() {
      edges.forEach(function (e, k) {
        var s = stateOf(k);
        marks[k].forEach(function (m) {
          m.setAttribute('stroke', ink[s]);
          if (m.tagName.toLowerCase() === 'polygon') m.setAttribute('fill', ink[s]);
          else if (s === 'blocked') m.setAttribute('stroke-dasharray', '3,3');
          else m.removeAttribute('stroke-dasharray');
          m.style.opacity = focus !== null && s === 'none' ? '0.25' : '';
          m.style.strokeWidth = focus !== null && s !== 'none' ? '2px' : '';
        });
      });
      boxes.forEach(function (b, i) {
        if (b) b.setAttribute('stroke', adjust.indexOf(nodes[i].name) >= 0 ? '#1F1F1F' : 'none');
      });
    }
    function names(v) {
      return v.length ? v.map(function (n) { return '<b>' + escapeHtml(label(n)) + '</b>'; }).join(', ') : '<b>nothing</b>';
    }
    function pathHtml(p) {
      var out = escapeHtml(label(p[0]));
      for (var i = 1; i < p.length; i++) {
        out += (arrow(p[i - 1], p[i]) ? ' \u2192 ' : ' \u2190 ') + escapeHtml(label(p[i]));
      }
      return out;
    }
    function render(msg) {
      var sets2 = sets;
      var verdict = result.sufficient ?
        'Adjusting for ' + names(adjust) + ' is <b>sufficient</b>: every biasing path from ' +
          escapeHtml(label(x)) + ' to ' + escapeHtml(label(y)) + ' is blocked.' :
        'Adjusting for ' + names(adjust) + ' is <b>not sufficient</b>: ' + [
          result.openBias ? result.openBias + (result.openBias === 1 ? ' biasing path stays open' : ' biasing paths stay open') : '',
          result.mediators.length ? names(result.mediators) + (result.mediators.length === 1 ? ' is a consequence' : ' are consequences') +
            ' of the exposure and should not be adjusted for' : ''
        ].filter(Boolean).join(', and ') + '.';
      var toggles = nodes.filter(function (n) { return n.name !== x && n.name !== y; }).map(function (n) {
        var on = adjust.indexOf(n.name) >= 0;
        return '<button type="button" class="ggx-chip" data-name="' + escapeHtml(n.name) + '" aria-pressed="' + on + '"' +
          (n.hidden ? ' disabled title="Unobserved, so it cannot be adjusted for"' : '') + '>' +
          escapeHtml(n.label) + (n.hidden ? ' <i>unobserved</i>' : '') + '</button>';
      }).join('');
      var rows = result.rows.map(function (r, j) {
        var kind = r.causal ? 'causal' : 'biasing';
        return '<li class="ggx-path ggx-path-' + (r.open ? kind : 'blocked') + '" data-j="' + j + '" tabindex="0">' +
          '<span class="ggx-path-tag">' + (r.open ? 'Open' : 'Blocked') + ', ' + kind + '</span>' +
          '<span class="ggx-path-text">' + pathHtml(r.path) + '</span>' +
          '<span class="ggx-path-why">' + escapeHtml(r.why.charAt(0).toUpperCase() + r.why.slice(1)) + '.</span></li>';
      }).join('');
      var setsHtml = sets2 === null ? 'Too many variables to search for minimal sets.' :
        !sets2.length ? 'No set of observed variables blocks every biasing path: the effect cannot be identified by adjustment in this diagram.' :
        sets2.map(function (z, k) {
          return '<button type="button" class="ggx-nomo-reset" data-set="' + k + '">' +
            (z.length ? 'Adjust for ' + z.map(function (v) { return escapeHtml(label(v)); }).join(' and ') : 'Adjust for nothing') + '</button>';
        }).join(' ');
      panel.innerHTML =
        '<p class="ggx-verdict ' + (result.sufficient ? 'ggx-ok' : 'ggx-bad') + '">' + verdict + '</p>' +
        (msg ? '<p class="ggx-paths-msg">' + msg + '</p>' : '') +
        '<div class="ggx-paths-row"><span class="ggx-paths-k">Adjusted for</span>' + toggles +
        (adjust.length ? ' <button type="button" class="ggx-link" data-clear="1">Clear</button>' : '') + '</div>' +
        '<div class="ggx-paths-k">' + (result.rows.length === 1 ? 'The one path' : 'The ' + result.rows.length + ' paths') +
        ' between ' + escapeHtml(label(x)) + ' and ' + escapeHtml(label(y)) + ' (hover to find one in the diagram)</div>' +
        (result.rows.length ? '<ul class="ggx-path-list">' + rows + '</ul>' : '<p>None: the two are not connected.</p>') +
        '<div class="ggx-paths-row"><span class="ggx-paths-k">Minimal sufficient sets</span>' + setsHtml + '</div>' +
        '<div class="ggx-paths-legend">' +
          '<span><svg width="26" height="8"><line x1="1" y1="4" x2="25" y2="4" stroke="' + ink.causal + '" stroke-width="2"/></svg>Open causal path</span>' +
          '<span><svg width="26" height="8"><line x1="1" y1="4" x2="25" y2="4" stroke="' + ink.biasing + '" stroke-width="2"/></svg>Open biasing path</span>' +
          '<span><svg width="26" height="8"><line x1="1" y1="4" x2="25" y2="4" class="ggx-paths-blocked" stroke-width="2" stroke-dasharray="3,3"/></svg>Blocked path</span>' +
          '<span><svg width="20" height="14"><rect x="1.5" y="1.5" width="17" height="11" rx="3" fill="none" class="ggx-paths-box" stroke-width="1.6"/></svg>Adjusted for</span>' +
        '</div>' +
        '<p class="ggx-readout-note">A path is causal when every arrow on it points away from the exposure, and biasing otherwise. ' +
        'Adjusting for a variable blocks a path through it, except at a collider, where two arrows meet: a collider blocks the path ' +
        'until it or one of its consequences is adjusted for. A set is sufficient when it blocks every biasing path and holds no ' +
        'consequence of the exposure (the backdoor criterion). The verdict is only as good as the arrows drawn, and says nothing ' +
        'about the size of the effect.</p>';
      live.textContent = panel.querySelector('.ggx-verdict').textContent;
    }
    function update(msg) {
      paths = allPaths(x, y);
      result = assess(x, y, adjust, paths);
      sets = minimalSets(x, y, paths);
      focus = null;
      render(msg);
      paint();
    }
    function toggle(name) {
      var n = nodes[at[name]];
      if (name === x || name === y) {
        update(escapeHtml(n.label) + ' is the ' + (name === x ? 'exposure' : 'outcome') + '; pick another variable to adjust for.');
      } else if (n.hidden) {
        update(escapeHtml(n.label) + ' is unobserved, so it cannot be adjusted for.');
      } else {
        var k = adjust.indexOf(name);
        if (k >= 0) adjust.splice(k, 1); else adjust.push(name);
        update();
      }
    }

    bar.addEventListener('change', function (ev) {
      var t = ev.target;
      if (!t.matches('select')) return;
      var other = t.getAttribute('data-k') === 'x' ? y : x;
      if (t.value === other) {
        t.value = t.getAttribute('data-k') === 'x' ? x : y;
        update('The exposure and the outcome must be different variables.');
        return;
      }
      if (t.getAttribute('data-k') === 'x') x = t.value; else y = t.value;
      adjust = adjust.filter(function (v) { return v !== x && v !== y; });
      update();
    });
    [].slice.call(bar.querySelectorAll('[data-mode]')).forEach(function (b, _, all) {
      b.addEventListener('click', function () {
        mode = b.getAttribute('data-mode');
        all.forEach(function (o) { o.setAttribute('aria-pressed', o === b); });
        svg.classList.toggle('ggx-adjusting', mode === 'adjust');
      });
    });
    svg.classList.add('ggx-adjusting');
    // In adjust mode a click on a variable adjusts for it instead of
    // opening its rationale; arrows still open theirs.
    svg.addEventListener('click', function (ev) {
      if (mode !== 'adjust') return;
      var t = ev.target.closest ? ev.target.closest('[data-id]') : null;
      var m = t && /^n(\d+)$/.exec(t.getAttribute('data-id'));
      if (!m) return;
      ev.stopPropagation();
      ev.preventDefault();
      toggle(nodes[Number(m[1]) - 1].name);
    }, true);
    panel.addEventListener('click', function (ev) {
      var t = ev.target.closest('button');
      if (!t) return;
      if (t.hasAttribute('data-name')) toggle(t.getAttribute('data-name'));
      else if (t.hasAttribute('data-clear')) { adjust = []; update(); }
      else if (t.hasAttribute('data-set')) { adjust = sets[Number(t.getAttribute('data-set'))].slice(); update(); }
    });
    function spot(ev) {
      var li = ev.target.closest ? ev.target.closest('.ggx-path') : null;
      var j = li ? Number(li.getAttribute('data-j')) : null;
      if (j === focus) return;
      focus = j;
      paint();
    }
    panel.addEventListener('mouseover', spot);
    panel.addEventListener('focusin', spot);
    panel.addEventListener('mouseleave', function () { focus = null; paint(); });
    panel.addEventListener('focusout', function (ev) { if (!panel.contains(ev.relatedTarget)) { focus = null; paint(); } });
    update();
  };

  // Where the evidence for a comparison in a network plot comes from: a
  // menu picks the comparison, and each line then takes a width and a label
  // for the share of that network estimate flowing through it.
  window.ggextremeNetFlow = function (el, data) {
    var svg = el.querySelector('svg');
    if (!svg || !data || !data.comparisons) return;
    var NS = 'http://www.w3.org/2000/svg';
    function visible(id) {
      return [].slice.call(svg.querySelectorAll('[data-id="' + id + '"]')).filter(function (m) {
        return m.getAttribute('stroke-opacity') !== '0.01' && m.tagName.toLowerCase() !== 'text';
      });
    }
    var lines = {};
    data.comparisons.forEach(function (c) {
      c.sources.forEach(function (s) {
        if (!lines[s.edge]) {
          lines[s.edge] = visible(s.edge).map(function (m) {
            return { m: m, stroke: m.getAttribute('stroke'), width: m.getAttribute('stroke-width') };
          });
        }
      });
    });
    var allLines = [].slice.call(svg.querySelectorAll('[data-id^="e"]')).filter(function (m) {
      return m.getAttribute('stroke-opacity') !== '0.01';
    });
    var layer = document.createElementNS(NS, 'g');
    layer.setAttribute('class', 'ggx-flow');
    svg.appendChild(layer);
    var bar = addControls(el, '<label class="ggx-range ggx-wrap">Show where the evidence comes from for ' +
      '<select class="ggx-select"><option value="">choose a comparison</option>' +
      data.comparisons.map(function (c, i) {
        return '<option value="' + i + '">' + escapeHtml(c.label) + '</option>';
      }).join('') + '</select></label>' +
      '<button type="button" class="ggx-nomo-reset" hidden>Show the whole network</button>');
    var readout = document.createElement('div');
    readout.className = 'ggx-readout';
    readout.setAttribute('aria-live', 'polite');
    el.insertBefore(readout, bar.nextSibling);
    var select = bar.querySelector('select');
    var clear = bar.querySelector('button');
    var ends = [];
    function pct(v) { return (100 * v).toFixed(v < 0.01 ? 1 : 0) + '%'; }
    function reset() {
      while (layer.firstChild) layer.removeChild(layer.firstChild);
      Object.keys(lines).forEach(function (id) {
        lines[id].forEach(function (l) { l.m.setAttribute('stroke', l.stroke); l.m.setAttribute('stroke-width', l.width); });
      });
      allLines.forEach(function (m) { m.style.opacity = ''; });
      ends.forEach(function (e) { e.m.setAttribute('stroke', e.stroke); e.m.setAttribute('stroke-width', e.width); });
      ends = [];
    }
    function show(i) {
      reset();
      clear.hidden = i === '';
      if (i === '') {
        readout.innerHTML = '<p class="ggx-readout-note">Pick a comparison to see which lines of the network its estimate draws on, and how much of it flows through each.</p>';
        return;
      }
      var c = data.comparisons[Number(i)];
      var used = {};
      c.sources.forEach(function (s) { used[s.edge] = s; });
      allLines.forEach(function (m) { if (!used[m.getAttribute('data-id')]) m.style.opacity = '0.18'; });
      c.sources.forEach(function (s) {
        (lines[s.edge] || []).forEach(function (l) {
          l.m.setAttribute('stroke', '#22928F');
          l.m.setAttribute('stroke-width', (1.5 + 14 * s.share).toFixed(2));
        });
        var path = (lines[s.edge] || [])[0];
        if (!path || !path.m.getTotalLength || s.share < 0.005) return;
        var mid = path.m.getPointAtLength(path.m.getTotalLength() / 2);
        var t = document.createElementNS(NS, 'text');
        t.setAttribute('x', mid.x); t.setAttribute('y', mid.y + 3.5);
        t.setAttribute('text-anchor', 'middle');
        t.setAttribute('class', 'ggx-flow-pct');
        t.textContent = pct(s.share);
        layer.appendChild(t);
      });
      [c.a, c.b].forEach(function (id) {
        var node = svg.querySelector('polygon[data-id="' + id + '"], path[data-id="' + id + '"]');
        if (!node) return;
        ends.push({ m: node, stroke: node.getAttribute('stroke'), width: node.getAttribute('stroke-width') });
        node.setAttribute('stroke', '#1F1F1F');
        node.setAttribute('stroke-width', '3');
      });
      var rows = c.sources.map(function (s) {
        return '<tr><th scope="row">' + escapeHtml(s.label) + '</th><td>' + s.studies + '</td><td>' +
          '<span class="ggx-flow-bar"><i style="width:' + (100 * s.share).toFixed(1) + '%"></i></span> ' + pct(s.share) + '</td></tr>';
      }).join('');
      readout.innerHTML = '<p><b>' + escapeHtml(c.label) + '</b>: ' +
        (c.direct > 0 ? pct(c.direct) + ' of the network estimate flows through the trials that compare the two directly, and ' +
          pct(1 - c.direct) + ' through other comparisons.' :
          'no trial compares the two directly, so all of the network estimate flows through other comparisons.') + '</p>' +
        '<div class="ggx-table ggx-flow-table"><table><thead><tr><td></td><th scope="col">Studies</th><th scope="col">Share of the estimate</th></tr></thead>' +
        '<tbody class="ggx-num">' + rows + '</tbody></table></div>' +
        '<p class="ggx-readout-note">' + escapeHtml(data.method) + '</p>';
    }
    select.addEventListener('change', function () { show(select.value); });
    clear.addEventListener('click', function () { select.value = ''; show(''); select.focus(); });
    show('');
  };

  // A diagnostic threshold explorer. Dragging the cutoff on the
  // distributions, or moving either slider, recomputes every measure with
  // the same formulas as dx_measures() in R/diagnostic.R and redraws the
  // shaded tails, the points, the curves, the grid of people and the table.
  window.ggextremeDiagnostic = function (el, data) {
    var svg = el.querySelector('svg');
    if (!svg || !data) return;
    el.classList.add('ggx-dx');
    var NS = 'http://www.w3.org/2000/svg';
    var h = data.bottom - data.top;
    function SX(v) { return data.dx + v; }
    function SY(v) { return data.dy + v; }
    function X(v) { return SX(data.a0 + (v - data.lo) / (data.hi - data.lo) * (data.a1 - data.a0)); }
    function R(v) { return SX(data.b0 + v * (data.b1 - data.b0)); }
    function Ry(v) { return SY(data.bottom - v * h); }
    function P(v) { return SX(data.c0 + (Math.log(v) - Math.log(data.plo)) / (Math.log(data.phi) - Math.log(data.plo)) * (data.c1 - data.c0)); }
    function byId(id) { return [].slice.call(svg.querySelectorAll('[data-id="' + id + '"]')); }
    var z = data.z, n1 = data.x1.length, n0 = data.x0.length, op = data.sign > 0 ? '\u2265' : '\u2264';
    // How many values are at or beyond a cutoff in the positive direction.
    function beyond(xs, c) {
      var lo = 0, hi = xs.length;
      if (data.sign > 0) {
        while (lo < hi) { var m = (lo + hi) >> 1; if (xs[m] < c) lo = m + 1; else hi = m; }
        return xs.length - lo;
      }
      while (lo < hi) { var k = (lo + hi) >> 1; if (xs[k] <= c) lo = k + 1; else hi = k; }
      return lo;
    }
    function wilson(k, n) {
      var p = k / n, mid = (p + z * z / (2 * n)) / (1 + z * z / n);
      var half = z * Math.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / (1 + z * z / n);
      return [p, mid - half, mid + half];
    }
    function logit(p) { return Math.log(p / (1 - p)); }
    function expit(v) { return 1 / (1 + Math.exp(-v)); }
    function measures(cut, prev) {
      var tp = beyond(data.x1, cut), fp = beyond(data.x0, cut), fn = n1 - tp, tn = n0 - fp;
      var se = tp / n1, sp = tn / n0, inner = se > 0 && se < 1 && sp > 0 && sp < 1;
      var ppv = se * prev / (se * prev + (1 - sp) * (1 - prev));
      var npv = sp * (1 - prev) / (sp * (1 - prev) + (1 - se) * prev);
      function lci(est, v) {
        if (!inner) return [est, NaN, NaN];
        return [est, expit(logit(est) - z * Math.sqrt(v)), expit(logit(est) + z * Math.sqrt(v))];
      }
      function gci(est, ok, v) {
        if (!ok || !isFinite(est) || est <= 0) return [est, NaN, NaN];
        return [est, Math.exp(Math.log(est) - z * Math.sqrt(v)), Math.exp(Math.log(est) + z * Math.sqrt(v))];
      }
      return {
        tp: tp, fp: fp, fn: fn, tn: tn,
        se: wilson(tp, n1), sp: wilson(tn, n0),
        ppv: lci(ppv, (1 - se) / (se * n1) + sp / ((1 - sp) * n0)),
        npv: lci(npv, se / ((1 - se) * n1) + (1 - sp) / (sp * n0)),
        lrp: gci(se / (1 - sp), tp && fp, 1 / tp - 1 / n1 + 1 / fp - 1 / n0),
        lrn: gci((1 - se) / sp, fn && tn, 1 / fn - 1 / n1 + 1 / tn - 1 / n0)
      };
    }
    function pct(v) { return isFinite(v) ? (100 * v).toFixed(v > 0.95 || v < 0.05 ? 1 : 0) + '%' : ''; }
    function prevFmt(v) { return v >= 0.1 ? pct(v) : (100 * v).toPrecision(2) + '%'; }
    function num(v) { return isFinite(v) ? v.toFixed(2) : ''; }
    function cutFmt(v) { return String(Math.round(v * 1e6) / 1e6); }
    function setText(id, v) { byId(id).forEach(function (t) { t.textContent = v; }); }
    function setLine(id, x1, y1, x2, y2) {
      byId(id).forEach(function (l) {
        if (l.tagName.toLowerCase() === 'line') {
          l.setAttribute('x1', x1); l.setAttribute('y1', y1); l.setAttribute('x2', x2); l.setAttribute('y2', y2);
        } else {
          l.setAttribute('points', x1 + ',' + y1 + ' ' + x2 + ',' + y2);
        }
      });
    }
    function setPoint(id, x, y) {
      byId(id).forEach(function (c) { c.setAttribute('cx', x); c.setAttribute('cy', y); });
    }

    // The tails beyond the cutoff: copies of the full densities, clipped.
    var defs = svg.querySelector('defs') || svg.insertBefore(document.createElementNS(NS, 'defs'), svg.firstChild);
    var clipId = 'ggx-dx-clip-' + Math.random().toString(36).slice(2, 8);
    var clip = document.createElementNS(NS, 'clipPath');
    clip.setAttribute('id', clipId);
    var clipRect = document.createElementNS(NS, 'rect');
    clipRect.setAttribute('y', 0); clipRect.setAttribute('height', 100000);
    clip.appendChild(clipRect);
    defs.appendChild(clip);
    ['dt0', 'dt1'].forEach(function (id) { byId(id).forEach(function (t) { t.style.display = 'none'; }); });
    ['dd0', 'dd1'].forEach(function (id, k) {
      byId(id).forEach(function (full) {
        var tail = full.cloneNode(true);
        tail.removeAttribute('data-id');
        tail.removeAttribute('id');
        tail.setAttribute('fill-opacity', k ? '0.45' : '0.4');
        tail.setAttribute('clip-path', 'url(#' + clipId + ')');
        tail.setAttribute('class', 'ggx-dx-tail');
        full.parentNode.insertBefore(tail, full.nextSibling);
      });
    });
    // The people, drawn here in place of the static grid.
    byId('grid').forEach(function (c) { c.style.display = 'none'; });
    var people = document.createElementNS(NS, 'g');
    people.setAttribute('class', 'ggx-dx-people');
    svg.appendChild(people);
    var cells = [];
    for (var i = 0; i < 1000; i++) {
      var r = document.createElementNS(NS, 'rect');
      r.setAttribute('x', SX(data.grid_x0 + (i % data.cols) * data.cell));
      r.setAttribute('y', SY(data.grid_top + Math.floor(i / data.cols) * data.cell));
      r.setAttribute('width', data.cell - data.cell_gap);
      r.setAttribute('height', data.cell - data.cell_gap);
      people.appendChild(r);
      cells.push(r);
    }

    var cuts = data.cuts;
    function nearest(v) {
      var best = 0;
      for (var k = 1; k < cuts.length; k++) if (Math.abs(cuts[k] - v) < Math.abs(cuts[best] - v)) best = k;
      return best;
    }
    var cut = data.cutoff, prev = data.prev;
    var lp = Math.log(0.001), hp = Math.log(0.9);
    function prevPos(p) { return Math.round((Math.log(Math.min(0.9, Math.max(0.001, p))) - lp) / (hp - lp) * 1000); }
    var first = data.prespecified ? 'Prespecified cutoff' : 'Youden cutoff';
    var bar = addControls(el,
      '<label class="ggx-range">Cutoff ' + op + ' <input type="range" data-k="cut" min="0" max="' + (cuts.length - 1) +
      '" step="1" value="' + nearest(cut) + '"> <b data-o="cut"></b></label>' +
      '<label class="ggx-range">Prevalence <input type="range" data-k="prev" min="0" max="1000" step="1" value="' +
      prevPos(prev) + '"> <b data-o="prev"></b></label>' +
      '<button type="button" class="ggx-nomo-reset" data-r="cut">' + first + '</button>' +
      (data.prespecified ? '<button type="button" class="ggx-nomo-reset" data-r="youden">Youden cutoff</button>' : '') +
      '<button type="button" class="ggx-nomo-reset" data-r="prev">Prevalence in the data</button>');
    var readout = document.createElement('div');
    readout.className = 'ggx-readout';
    readout.setAttribute('aria-live', 'polite');
    // The plot is tall, so the controls and the sentence sit above it.
    el.insertBefore(bar, el.firstChild);
    el.insertBefore(readout, bar.nextSibling);
    var inCut = bar.querySelector('[data-k="cut"]'), inPrev = bar.querySelector('[data-k="prev"]');

    function draw() {
      var m = measures(cut, prev);
      bar.querySelector('[data-o="cut"]').textContent = cutFmt(cut);
      bar.querySelector('[data-o="prev"]').textContent = prevFmt(prev);
      var xc = X(cut);
      if (data.sign > 0) { clipRect.setAttribute('x', xc); clipRect.setAttribute('width', 100000); }
      else { clipRect.setAttribute('x', 0); clipRect.setAttribute('width', xc); }
      setLine('dcut', xc, SY(data.top - 4), xc, SY(data.bottom));
      byId('dcl').forEach(function (t) { t.setAttribute('x', xc + 4); t.textContent = op + ' ' + cutFmt(cut); });
      var se = m.se[0], sp = m.sp[0];
      setPoint('rp', R(1 - sp), Ry(se));
      setLine('rpy', R(1 - sp), Ry(m.se[1]), R(1 - sp), Ry(m.se[2]));
      setLine('rpx', R(1 - m.sp[2]), Ry(se), R(1 - m.sp[1]), Ry(se));
      setText('rpl', 'Sensitivity ' + pct(se) + ', specificity ' + pct(sp));
      var pts1 = [], pts2 = [];
      for (var k = 0; k < 120; k++) {
        var q = Math.exp(Math.log(data.plo) + k / 119 * (Math.log(data.phi) - Math.log(data.plo)));
        var a = se * q / (se * q + (1 - sp) * (1 - q)), b = sp * (1 - q) / (sp * (1 - q) + (1 - se) * q);
        pts1.push(P(q).toFixed(1) + ',' + Ry(isFinite(a) ? a : 0).toFixed(1));
        pts2.push(P(q).toFixed(1) + ',' + Ry(isFinite(b) ? b : 1).toFixed(1));
      }
      byId('pc1').forEach(function (l) { l.setAttribute('points', pts1.join(' ')); });
      byId('pc2').forEach(function (l) { l.setAttribute('points', pts2.join(' ')); });
      setLine('pl', P(prev), SY(data.top), P(prev), SY(data.bottom));
      setPoint('pp', P(prev), Ry(isFinite(m.ppv[0]) ? m.ppv[0] : 0));
      setPoint('pn', P(prev), Ry(isFinite(m.npv[0]) ? m.npv[0] : 1));
      byId('plt').forEach(function (t) {
        var right = P(prev) > SX(data.c1) - 70;
        t.setAttribute('x', right ? P(prev) - 4 : P(prev) + 4);
        t.setAttribute('text-anchor', right ? 'end' : 'start');
        t.textContent = 'Prevalence ' + prevFmt(prev);
      });
      // People: found, missed, false alarms, correctly cleared.
      var nd = Math.round(1000 * prev), found = Math.round(nd * se), alarms = Math.round((1000 - nd) * (1 - sp));
      cells.forEach(function (c, i) {
        var kind = i < found ? 'found' : i < nd ? 'missed' : i < nd + alarms ? 'false_alarm' : 'cleared';
        c.setAttribute('fill', kind === 'missed' ? data.ink.found : data.ink[kind]);
        c.setAttribute('fill-opacity', kind === 'missed' ? '0.3' : '1');
      });
      var lead = (data.level * 100) + '% CI ';
      function ci(v, f) { return isFinite(v[1]) ? lead + f(v[1]) + ' to ' + f(v[2]) : 'interval not estimable'; }
      var cellsTxt = [
        [pct(se), m.tp + ' of ' + n1 + '; ' + ci(m.se, pct)],
        [pct(sp), m.tn + ' of ' + n0 + '; ' + ci(m.sp, pct)],
        [pct(m.ppv[0]), ci(m.ppv, pct)], [pct(m.npv[0]), ci(m.npv, pct)],
        [num(m.lrp[0]), ci(m.lrp, num)], [num(m.lrn[0]), ci(m.lrn, num)],
        [num(data.auc.auc), lead + num(data.auc.lower) + ' to ' + num(data.auc.upper) + '; over all cutoffs'],
        [op + ' ' + cutFmt(cut), Math.abs(cut - data.cutoff) < 1e-9 && data.prespecified ? 'prespecified' :
          Math.abs(cut - data.youden) < 1e-9 ? 'chosen in these data (Youden)' : 'explored here']
      ];
      cellsTxt.forEach(function (v, k) { setText('sv' + (k + 1), v[0]); setText('su' + (k + 1), v[1]); });
      var name = data.labels[1].toLowerCase();
      var sentence = 'Of 1,000 people tested where ' + nd + ' have ' + name + ', a cutoff of ' + op + ' ' + cutFmt(cut) +
        ' finds ' + found + ' and misses ' + (nd - found) + ', and raises ' + alarms + ' false alarms. A positive result means ' +
        name + ' ' + pct(m.ppv[0]) + ' of the time.';
      setText('gl', sentence);
      setText('gh', '1,000 people tested at a prevalence of ' + prevFmt(prev) +
        (Math.abs(prev - data.sample_prev) < 1e-9 ? ', as in these data' : ''));
      readout.innerHTML = '<p>' + escapeHtml(sentence) + ' A negative result rules it out ' + pct(m.npv[0]) + ' of the time.</p>' +
        '<p class="ggx-readout-note">' +
        (Math.abs(prev - data.sample_prev) > 1e-9 ? 'The prevalence changes the predictive values and the people, not sensitivity or specificity. ' : '') +
        (data.prespecified && Math.abs(cut - data.cutoff) > 1e-9 ? 'This cutoff was not prespecified: one chosen by looking at these data will do less well in new patients. ' :
          !data.prespecified ? 'A cutoff chosen by looking at these data will do less well in new patients. ' : '') +
        'Drag the cutoff on the distributions, or use the sliders.</p>';
    }

    inCut.addEventListener('input', function () { cut = cuts[Number(inCut.value)]; draw(); });
    inPrev.addEventListener('input', function () {
      prev = Math.exp(lp + Number(inPrev.value) / 1000 * (hp - lp)); draw();
    });
    bar.addEventListener('click', function (ev) {
      var b = ev.target.closest('[data-r]');
      if (!b) return;
      var r = b.getAttribute('data-r');
      if (r === 'cut') cut = data.cutoff;
      if (r === 'youden') cut = data.youden;
      if (r === 'prev') prev = data.sample_prev;
      inCut.value = nearest(cut);
      inPrev.value = prevPos(prev);
      draw();
    });
    // Drag the cutoff on the distributions.
    var hit = document.createElementNS(NS, 'rect');
    hit.setAttribute('x', SX(data.a0)); hit.setAttribute('y', SY(data.top));
    hit.setAttribute('width', data.a1 - data.a0); hit.setAttribute('height', h);
    hit.setAttribute('class', 'ggx-dx-drag');
    svg.appendChild(hit);
    var dragging = false;
    function fromPointer(ev) {
      var pt = svg.createSVGPoint();
      pt.x = ev.clientX; pt.y = ev.clientY;
      var loc = pt.matrixTransform(svg.getScreenCTM().inverse());
      var v = data.lo + (loc.x - SX(data.a0)) / (data.a1 - data.a0) * (data.hi - data.lo);
      var k = nearest(v);
      cut = cuts[k];
      inCut.value = k;
      draw();
    }
    hit.addEventListener('pointerdown', function (ev) { dragging = true; hit.setPointerCapture(ev.pointerId); fromPointer(ev); });
    hit.addEventListener('pointermove', function (ev) { if (dragging) fromPointer(ev); });
    hit.addEventListener('pointerup', function () { dragging = false; });
    hit.addEventListener('pointercancel', function () { dragging = false; });
    draw();
  };

  // A bias and tipping point explorer: clicking the surface, or moving the
  // sliders, chooses the strengths of an unmeasured confounder, and the
  // marker, the adjusted row and the sentence follow, by the bounding
  // factor of Ding and VanderWeele as in sens_rows() in R/sensitivity.R.
  window.ggextremeSensitivity = function (el, data) {
    var svg = el.querySelector('svg');
    if (!svg || !data) return;
    el.classList.add('ggx-sens');
    var NS = 'http://www.w3.org/2000/svg';
    var M = data.top;
    function SX(v) { return data.dx + v; }
    function SY(v) { return data.dy + v; }
    function X(a) { return SX(data.s0 + (a - 1) / (M - 1) * (data.s1 - data.s0)); }
    function Y(b) { return SY(data.sbottom - (b - 1) / (M - 1) * (data.sbottom - data.stop)); }
    function F(v) { return SX(data.f0 + (Math.log(v) - Math.log(data.lo)) / (Math.log(data.hi) - Math.log(data.lo)) * (data.f1 - data.f0)); }
    function byId(id) { return [].slice.call(svg.querySelectorAll('[data-id="' + id + '"]')); }
    function bias(a, b) { return a * b / (a + b - 1); }
    function unflip(v) { return data.up ? v : 1 / v; }
    function n2(v) { return v.toFixed(2); }
    function clamp(v) { return Math.min(data.hi, Math.max(data.lo, v)); }
    var a = data.chosen[0], b = data.chosen[1];
    var bar = addControls(el,
      '<label class="ggx-range">With the exposure <input type="range" data-k="a" min="1" max="' + M + '" step="0.05" value="' + a + '"> <b data-o="a"></b></label>' +
      '<label class="ggx-range">With the outcome <input type="range" data-k="b" min="1" max="' + M + '" step="0.05" value="' + b + '"> <b data-o="b"></b></label>');
    var readout = document.createElement('div');
    readout.className = 'ggx-readout';
    readout.setAttribute('aria-live', 'polite');
    el.insertBefore(readout, bar.nextSibling);
    var inA = bar.querySelector('[data-k="a"]'), inB = bar.querySelector('[data-k="b"]');
    function draw() {
      bar.querySelector('[data-o="a"]').textContent = n2(a);
      bar.querySelector('[data-o="b"]').textContent = n2(b);
      byId('mk').forEach(function (c) { c.setAttribute('cx', X(a)); c.setAttribute('cy', Y(b)); });
      var B = bias(a, b);
      var est = unflip(data.e / B), l1 = unflip(data.limit / B), l2 = unflip(data.far / B);
      var lo = Math.min(l1, l2), hi = Math.max(l1, l2);
      byId('chb').forEach(function (l) {
        var y = l.getAttribute('y1') || l.getAttribute('points').split(/[ ,]/)[1];
        if (l.tagName.toLowerCase() === 'line') { l.setAttribute('x1', F(clamp(lo))); l.setAttribute('x2', F(clamp(hi))); }
        else l.setAttribute('points', F(clamp(lo)) + ',' + y + ' ' + F(clamp(hi)) + ',' + y);
      });
      byId('chp').forEach(function (c) { c.setAttribute('cx', F(clamp(est))); });
      byId('chv').forEach(function (t) { t.textContent = n2(est) + ' (' + n2(lo) + ' to ' + n2(hi) + ')'; });
      byId('chs').forEach(function (t) { t.textContent = n2(a) + ' with the exposure, ' + n2(b) + ' with the outcome'; });
      var clear = data.limit / B > 1, same = data.e / B > 1, imp = data.imp && data.e / B >= data.imp;
      var verdict = !same ? 'it could explain the estimate away entirely' :
        !clear ? 'the estimate stays on the same side of 1, but the interval reaches it' :
        data.imp && !imp ? 'an effect remains, but it may no longer be clinically important' :
        data.imp ? 'the effect stays clinically important, with an interval clear of 1' : 'the interval stays clear of 1';
      readout.innerHTML = '<p>An unmeasured confounder with a risk ratio of <b>' + n2(a) + '</b> with the exposure and <b>' + n2(b) +
        '</b> with the outcome could divide the estimate by at most ' + n2(B) + ', to <b>' + n2(est) + '</b> (' + n2(lo) + ' to ' +
        n2(hi) + '): ' + verdict + '.</p><p class="ggx-readout-note">Click the surface or use the sliders to choose a confounder. ' +
        'Values are on the ' + escapeHtml(data.scale) + ' scale and are the worst case for each pair of strengths.</p>';
    }
    inA.addEventListener('input', function () { a = Number(inA.value); draw(); });
    inB.addEventListener('input', function () { b = Number(inB.value); draw(); });
    var hit = document.createElementNS(NS, 'rect');
    hit.setAttribute('x', SX(data.s0)); hit.setAttribute('y', SY(data.stop));
    hit.setAttribute('width', data.s1 - data.s0); hit.setAttribute('height', data.sbottom - data.stop);
    hit.setAttribute('class', 'ggx-sens-hit');
    svg.appendChild(hit);
    var dragging = false;
    function pick(ev) {
      var pt = svg.createSVGPoint();
      pt.x = ev.clientX; pt.y = ev.clientY;
      var loc = pt.matrixTransform(svg.getScreenCTM().inverse());
      a = Math.round(Math.min(M, Math.max(1, 1 + (loc.x - SX(data.s0)) / (data.s1 - data.s0) * (M - 1))) * 20) / 20;
      b = Math.round(Math.min(M, Math.max(1, 1 + (SY(data.sbottom) - loc.y) / (data.sbottom - data.stop) * (M - 1))) * 20) / 20;
      inA.value = a; inB.value = b;
      draw();
    }
    hit.addEventListener('pointerdown', function (ev) { dragging = true; hit.setPointerCapture(ev.pointerId); pick(ev); });
    hit.addEventListener('pointermove', function (ev) { if (dragging) pick(ev); });
    hit.addEventListener('pointerup', function () { dragging = false; });
    hit.addEventListener('pointercancel', function () { dragging = false; });
    draw();
  };

  // A multiverse of analyses. Dragging across the curve selects a run of
  // analyses; clicking a choice in the grid keeps only the analyses that
  // made it. The primary analysis is never hidden, and the readout says
  // what the analyses in view share.
  window.ggextremeMultiverse = function (el, data) {
    var svg = el.querySelector('svg');
    if (!svg || !data) return;
    el.classList.add('ggx-mv');
    var NS = 'http://www.w3.org/2000/svg';
    var n = data.est.length;
    var marks = [];
    for (var i = 0; i < n; i++) marks.push([].slice.call(svg.querySelectorAll('[data-id="s' + (i + 1) + '"]')));
    var byRank = [];
    data.rank.forEach(function (r, i) { byRank[r - 1] = i; });
    var filters = data.choices.map(function () { return []; });
    var brush = null;
    function f(v) { return v.toFixed(data.ratio && v >= 10 ? 1 : 2); }
    function passes(i) {
      return data.choices.every(function (ch, d) { return !filters[d].length || filters[d].indexOf(ch.of[i]) >= 0; });
    }
    function median(v) {
      var s = v.slice().sort(function (a, b) { return a - b; }), m = s.length >> 1;
      return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
    }
    var shade = document.createElementNS(NS, 'rect');
    shade.setAttribute('class', 'ggx-mv-brush');
    shade.setAttribute('y', data.dy + data.top);
    shade.setAttribute('height', data.bottom - data.top);
    shade.style.display = 'none';
    svg.insertBefore(shade, svg.firstChild.nextSibling);
    var bar = addControls(el, '<button type="button" class="ggx-nomo-reset" data-a="clear">Show every analysis</button>' +
      '<span>Drag across the curve to select analyses; click a choice in the grid to keep only the analyses that made it.</span>');
    var readout = document.createElement('div');
    readout.className = 'ggx-readout';
    readout.setAttribute('aria-live', 'polite');
    el.insertBefore(readout, bar.nextSibling);
    function paint() {
      var shown = [];
      for (var i = 0; i < n; i++) {
        var ok = passes(i) || data.primary[i];
        var inBrush = !brush || (data.rank[i] >= brush[0] && data.rank[i] <= brush[1]);
        var op = !ok ? '0.06' : inBrush ? '' : '0.22';
        marks[i].forEach(function (m) { m.style.opacity = op; });
        if (ok && inBrush) shown.push(i);
      }
      data.rows.forEach(function (r, j) {
        var on = filters[r.d - 1].indexOf(r.k) >= 0;
        var label = svg.querySelector('[data-id="o' + (j + 1) + '"]');
        if (label) label.classList.toggle('ggx-mv-on', on);
        var use = shown.filter(function (i) { return data.choices[r.d - 1].of[i] === r.k; });
        var med = svg.querySelector('[data-id="m' + (j + 1) + '"]');
        if (med) med.textContent = use.length ? f(median(use.map(function (i) { return data.est[i]; }))) : '';
      });
      if (brush) {
        var x0 = data.dx + data.x0 + (brush[0] - 1) * data.step, x1 = data.dx + data.x0 + brush[1] * data.step;
        shade.setAttribute('x', x0); shade.setAttribute('width', x1 - x0); shade.style.display = '';
      } else shade.style.display = 'none';
      if (!shown.length) { readout.innerHTML = '<p>No analysis made all of these choices.</p>'; return; }
      var est = shown.map(function (i) { return data.est[i]; });
      var excl = shown.filter(function (i) { return data.hi[i] < data.null || data.lo[i] > data.null; }).length;
      var shared = data.choices.map(function (ch) {
        var counts = ch.levels.map(function (_, k) { return shown.filter(function (i) { return ch.of[i] === k + 1; }).length; });
        var top = counts.indexOf(Math.max.apply(null, counts));
        if (counts[top] === shown.length) return escapeHtml(ch.name) + ': all ' + escapeHtml(String(ch.levels[top]));
        if (counts[top] / shown.length >= 0.75) return escapeHtml(ch.name) + ': mostly ' + escapeHtml(String(ch.levels[top]));
        return null;
      }).filter(Boolean);
      var active = data.choices.map(function (ch, d) {
        return filters[d].length ? escapeHtml(ch.name) + ' is ' + filters[d].map(function (k) { return escapeHtml(String(ch.levels[k - 1])); }).join(' or ') : null;
      }).filter(Boolean);
      readout.innerHTML = '<p><b>' + shown.length + ' of ' + n + ' analyses</b>' + (brush || active.length ? ' in view' : '') +
        ': median ' + escapeHtml(data.ylab.toLowerCase()) + ' ' + f(median(est)) + ', from ' + f(Math.min.apply(null, est)) + ' to ' +
        f(Math.max.apply(null, est)) + '; ' + excl + ' exclude ' + data.null + '.' +
        (brush && shared.length ? ' They share: ' + shared.join('; ') + '.' : brush ? ' No choice is shared by most of them.' : '') + '</p>' +
        (active.length ? '<p class="ggx-readout-note">Keeping only analyses where ' + active.join(', and ') + '. The primary analysis always stays in view.</p>' : '');
    }
    function rankAt(ev) {
      var pt = svg.createSVGPoint();
      pt.x = ev.clientX; pt.y = ev.clientY;
      var loc = pt.matrixTransform(svg.getScreenCTM().inverse());
      return { r: Math.min(n, Math.max(1, Math.ceil((loc.x - data.dx - data.x0) / data.step))), y: loc.y - data.dy, x: loc.x - data.dx };
    }
    var start = null, moved = false;
    svg.addEventListener('pointerdown', function (ev) {
      var at = rankAt(ev);
      if (at.x < data.x0 || at.y < data.top - 6 || at.y > data.bottom + 6) return;
      start = at.r; moved = false;
      svg.setPointerCapture(ev.pointerId);
      ev.preventDefault();
    });
    svg.addEventListener('pointermove', function (ev) {
      if (start === null) return;
      var r = rankAt(ev).r;
      if (r !== start) moved = true;
      if (moved) { brush = [Math.min(start, r), Math.max(start, r)]; paint(); }
    });
    svg.addEventListener('pointerup', function () {
      if (start !== null && !moved) { brush = null; paint(); }
      start = null;
    });
    svg.addEventListener('click', function (ev) {
      var t = ev.target.closest ? ev.target.closest('[data-id]') : null;
      var m = t && /^o(\d+)$/.exec(t.getAttribute('data-id'));
      if (!m) return;
      var r = data.rows[Number(m[1]) - 1], list = filters[r.d - 1], k = list.indexOf(r.k);
      if (k >= 0) list.splice(k, 1); else list.push(r.k);
      paint();
    });
    bar.querySelector('[data-a="clear"]').addEventListener('click', function () {
      filters = data.choices.map(function () { return []; });
      brush = null;
      paint();
    });
    paint();
  };

  // A responder threshold plot: a slider, or a drag on either panel, moves
  // the threshold, and the points, the interval and the table follow, with
  // the formulas of resp_at() and resp_nnt() in R/responder.R.
  window.ggextremeResponder = function (el, data) {
    var svg = el.querySelector('svg');
    if (!svg || !data) return;
    el.classList.add('ggx-resp');
    var NS = 'http://www.w3.org/2000/svg';
    var z = data.z, n1 = data.y1.length, n0 = data.y0.length;
    function SX(v) { return data.dx + v; }
    function SY(v) { return data.dy + v; }
    function X(v) { return SX(data.a0 + (v - data.lo) / (data.hi - data.lo) * (data.a1 - data.a0)); }
    function Xb(v) { return SX(data.b0 + (v - data.lo) / (data.hi - data.lo) * (data.b1 - data.b0)); }
    function Y(p) { return SY(data.bottom - p * (data.bottom - data.top)); }
    function Yd(v) { return SY(data.bottom - (v - data.dr[0]) / (data.dr[1] - data.dr[0]) * (data.bottom - data.top)); }
    function byId(id) { return [].slice.call(svg.querySelectorAll('[data-id="' + id + '"]')); }
    function atLeast(xs, t) {
      var lo = 0, hi = xs.length;
      while (lo < hi) { var m = (lo + hi) >> 1; if (xs[m] < t) lo = m + 1; else hi = m; }
      return xs.length - lo;
    }
    function wilson(k, n) {
      var p = k / n, mid = (p + z * z / (2 * n)) / (1 + z * z / n);
      var half = z * Math.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / (1 + z * z / n);
      return [p, mid - half, mid + half];
    }
    function pct(v) { return (100 * v).toFixed(0) + '%'; }
    function signed(v) { return (v > 0 ? '+' : v < 0 ? '\u2212' : '') + Math.abs(100 * v).toFixed(1); }
    function inv(v) { return (1 / Math.abs(v)).toFixed(1); }
    function setLine(id, x1, y1, x2, y2) {
      byId(id).forEach(function (l) {
        if (l.tagName.toLowerCase() === 'line') {
          l.setAttribute('x1', x1); l.setAttribute('y1', y1); l.setAttribute('x2', x2); l.setAttribute('y2', y2);
        } else l.setAttribute('points', x1 + ',' + y1 + ' ' + x2 + ',' + y2);
      });
    }
    function setPoint(id, x, y) { byId(id).forEach(function (c) { c.setAttribute('cx', x); c.setAttribute('cy', y); }); }
    function setText(id, v) { byId(id).forEach(function (t) { t.textContent = v; }); }
    var t = data.threshold;
    var step = (data.hi - data.lo) / 200;
    var bar = addControls(el, '<label class="ggx-range">Responder: improved by at least <input type="range" min="' + data.lo +
      '" max="' + data.hi + '" step="' + step + '" value="' + t + '"> <b></b></label>' +
      '<button type="button" class="ggx-nomo-reset">Prespecified threshold</button>');
    var readout = document.createElement('div');
    readout.className = 'ggx-readout';
    readout.setAttribute('aria-live', 'polite');
    el.insertBefore(readout, bar.nextSibling);
    var input = bar.querySelector('input');
    function fmtT(v) { return String(Math.round(v * 100) / 100); }
    function draw() {
      bar.querySelector('b').textContent = fmtT(t);
      var k1 = atLeast(data.y1, t), k0 = atLeast(data.y0, t);
      var w1 = wilson(k1, n1), w0 = wilson(k0, n0), d = w1[0] - w0[0];
      var lo = d - Math.sqrt(Math.pow(w1[0] - w1[1], 2) + Math.pow(w0[2] - w0[0], 2));
      var hi = d + Math.sqrt(Math.pow(w1[2] - w1[0], 2) + Math.pow(w0[0] - w0[1], 2));
      setLine('ta', X(t), SY(data.top), X(t), SY(data.bottom));
      setLine('tb', Xb(t), SY(data.top), Xb(t), SY(data.bottom));
      setLine('db', Xb(t), Yd(lo), Xb(t), Yd(hi));
      setPoint('pa', X(t), Y(w1[0]));
      setPoint('pc', X(t), Y(w0[0]));
      setPoint('pd', Xb(t), Yd(d));
      byId('tl').forEach(function (l) { l.setAttribute('x', X(t) + 4); l.textContent = '\u2265 ' + fmtT(t); });
      var lead = (data.level * 100) + '% CI ';
      var nnt, nntU;
      if (Math.abs(d) < 1e-12) { nnt = 'none'; nntU = 'no difference in responders'; }
      else {
        nnt = (d > 0 ? 'NNTB ' : 'NNTH ') + inv(d);
        nntU = lo > 0 ? lead + 'NNTB ' + inv(hi) + ' to ' + inv(lo) : hi < 0 ? lead + 'NNTH ' + inv(lo) + ' to ' + inv(hi) :
          lead + 'NNTB ' + inv(hi) + ' to \u221e to NNTH ' + inv(lo);
      }
      setText('rv1', pct(w1[0])); setText('ru1', k1 + ' of ' + n1 + '; ' + lead + pct(w1[1]) + ' to ' + pct(w1[2]));
      setText('rv2', pct(w0[0])); setText('ru2', k0 + ' of ' + n0 + '; ' + lead + pct(w0[1]) + ' to ' + pct(w0[2]));
      setText('rv3', signed(d)); setText('ru3', 'percentage points; ' + lead + signed(lo) + ' to ' + signed(hi));
      setText('rv4', nnt); setText('ru4', nntU);
      var sentence = 'With a threshold of ' + fmtT(t) + ', ' + pct(w1[0]) + ' respond on ' + data.arms[1] + ' and ' + pct(w0[0]) +
        ' on ' + data.arms[0] + ': a difference of ' + (100 * d).toFixed(1) + ' percentage points (' + (100 * lo).toFixed(1) + ' to ' +
        (100 * hi).toFixed(1) + ').';
      setText('sn', sentence);
      readout.innerHTML = '<p>' + escapeHtml(sentence) + (lo > 0 || hi < 0 ? '' : ' The interval includes no difference.') + '</p>' +
        '<p class="ggx-readout-note">' + (Math.abs(t - data.threshold) > 1e-9 ?
          'This threshold was not prespecified; choosing one after seeing the data can make any difference look real. ' : '') +
        'The mean difference, ' + data.md[0].toFixed(2) + ' (' + data.md[1].toFixed(2) + ' to ' + data.md[2].toFixed(2) +
        '), uses every patient and does not depend on the threshold.</p>';
    }
    input.addEventListener('input', function () { t = Number(input.value); draw(); });
    bar.querySelector('button').addEventListener('click', function () { t = data.threshold; input.value = t; draw(); });
    [[data.a0, data.a1], [data.b0, data.b1]].forEach(function (span) {
      var hit = document.createElementNS(NS, 'rect');
      hit.setAttribute('x', SX(span[0])); hit.setAttribute('y', SY(data.top));
      hit.setAttribute('width', span[1] - span[0]); hit.setAttribute('height', data.bottom - data.top);
      hit.setAttribute('class', 'ggx-resp-drag');
      svg.appendChild(hit);
      var dragging = false;
      function move(ev) {
        var pt = svg.createSVGPoint();
        pt.x = ev.clientX; pt.y = ev.clientY;
        var loc = pt.matrixTransform(svg.getScreenCTM().inverse());
        var v = data.lo + (loc.x - SX(span[0])) / (span[1] - span[0]) * (data.hi - data.lo);
        t = Math.min(data.hi, Math.max(data.lo, Math.round(v / step) * step));
        input.value = t;
        draw();
      }
      hit.addEventListener('pointerdown', function (ev) { dragging = true; hit.setPointerCapture(ev.pointerId); move(ev); });
      hit.addEventListener('pointermove', function (ev) { if (dragging) move(ev); });
      hit.addEventListener('pointerup', function () { dragging = false; });
      hit.addEventListener('pointercancel', function () { dragging = false; });
    });
    draw();
  };

  // CINeMA plots (R/cinema*.R). The shapes a reader clicks can also be
  // reached with the keyboard: the first shape of each id that starts with
  // `prefix` takes focus in reading order, and Enter or Space clicks it.
  window.ggextremeCinemaKeys = function (el, prefix) {
    var svg = el.querySelector('svg');
    if (!svg) return;
    var seen = {};
    [].slice.call(svg.querySelectorAll('[data-id^="' + prefix + '"]')).forEach(function (node) {
      var id = node.getAttribute('data-id');
      if (seen[id] || !/^[a-z]+[0-9_]+$/.test(id)) return;
      if (node.tagName.toLowerCase() === 'text') return;
      seen[id] = true;
      // ggiraph keeps the hover card in the title, written as escaped HTML.
      var raw = document.createElement('textarea');
      raw.innerHTML = node.getAttribute('title') || '';
      var box = document.createElement('div');
      box.innerHTML = raw.value;
      var head = box.querySelector('.ggx-tip-title');
      var sub = box.querySelector('.ggx-tip-sub');
      node.setAttribute('tabindex', '0');
      node.setAttribute('role', 'button');
      node.setAttribute('aria-label', ((head ? head.textContent : id) + (sub ? ', ' + sub.textContent : '')).trim());
      node.classList.add('ggx-cn-key');
      node.addEventListener('keydown', function (ev) {
        if (ev.key !== 'Enter' && ev.key !== ' ') return;
        ev.preventDefault();
        node.dispatchEvent(new MouseEvent('click', { bubbles: true }));
      });
    });
  };

  // The rules shared by the CINeMA plots, as in R/cinema.R, on the scale of
  // the analysis (log for ratios) with no effect at 0. zones(): which of the
  // three areas an interval reaches, 1 below the range, 2 within it, 4
  // above it. step(): 0 when an interval lies within the range or on the
  // point estimate's side of no effect, 1 when it crosses no effect but not
  // the limit beyond, 2 when it passes that limit too.
  var cinema = {
    zones: function (l, u, L, U) { return (l < L ? 1 : 0) + (l <= U && u >= L && U > L ? 2 : 0) + (u > U ? 4 : 0); },
    step: function (e, l, u, L, U) {
      if (l >= L && u <= U) return 0;
      if (e >= 0) return l >= 0 ? 0 : l >= L ? 1 : 2;
      return u <= 0 ? 0 : u <= U ? 1 : 2;
    },
    bits: function (m) { return (m & 1) + ((m >> 1) & 1) + ((m >> 2) & 1); },
    num: function (v, ratio) {
      var z = ratio ? Math.exp(v) : v;
      if (!isFinite(z)) return 'NA';
      if (!ratio) return z.toFixed(2);
      return z >= 100 ? z.toFixed(0) : z >= 10 ? z.toFixed(1) : z >= 0.1 ? z.toFixed(2) :
        z >= 0.01 ? z.toFixed(3) : z.toPrecision(2);
    },
    // Move a rectangle drawn as a rect or as a polygon to span x0 to x1.
    span: function (node, x0, x1) {
      if (node.tagName.toLowerCase() === 'rect') {
        node.setAttribute('x', Math.min(x0, x1));
        node.setAttribute('width', Math.max(0.5, Math.abs(x1 - x0)));
        return;
      }
      var b = node.getBBox();
      node.setAttribute('points', [x0, b.y, x1, b.y, x1, b.y + b.height, x0, b.y + b.height].join(' '));
    },
    line: function (node, x) {
      if (node.tagName.toLowerCase() === 'line') {
        node.setAttribute('x1', x); node.setAttribute('x2', x);
      } else {
        var p = node.getAttribute('points').split(/[ ,]+/);
        node.setAttribute('points', x + ',' + p[1] + ' ' + x + ',' + p[3]);
      }
    },
    tip: function (el) {
      if (window.getComputedStyle(el).position === 'static') el.style.position = 'relative';
      var tip = document.createElement('div');
      tip.className = 'ggx-float-tip';
      tip.hidden = true;
      el.appendChild(tip);
      return {
        show: function (html, ev) {
          tip.innerHTML = html;
          tip.hidden = false;
          var box = el.getBoundingClientRect();
          var x = ev.clientX - box.left + 14, y = ev.clientY - box.top + 14;
          if (x + tip.offsetWidth > box.width - 4) x = ev.clientX - box.left - tip.offsetWidth - 14;
          tip.style.left = Math.max(4, x) + 'px';
          tip.style.top = Math.max(4, y) + 'px';
        },
        hide: function () { tip.hidden = true; }
      };
    },
    point: function (svg, ev) {
      var p = svg.createSVGPoint();
      p.x = ev.clientX; p.y = ev.clientY;
      return p.matrixTransform(svg.getScreenCTM().inverse());
    }
  };

  // Network estimates against a range of little difference (R/cinema_clinical.R).
  // Two sliders, or a click on a sensitivity strip, move the lower and the
  // upper limit; the region, every row's readings and judgments, the strips
  // and the summary follow.
  window.ggextremeCinemaClinical = function (el, data) {
    var svg = el.querySelector('svg');
    if (!svg || !data) return;
    el.classList.add('ggx-cn-clin');
    var NS = 'http://www.w3.org/2000/svg';
    function byId(id) { return [].slice.call(svg.querySelectorAll('[data-id="' + id + '"]')); }
    function SX(v) { return data.dx + v; }
    function SY(v) { return data.dy + v; }
    function X(v) { return SX(data.f0 + (Math.min(Math.max(v, data.a), data.b) - data.a) / (data.b - data.a) * (data.f1 - data.f0)); }
    function BX(B, v) { return SX(B.x0 + (v - B.range[0]) / (B.range[1] - B.range[0]) * (B.x1 - B.x0)); }
    function num(v) { return cinema.num(v, data.ratio); }
    function shown(v) { return data.ratio ? Math.exp(v) : v; }
    function scaled(v) { return data.ratio ? Math.log(v) : v; }
    function setText(id, s) { byId(id).forEach(function (t) { t.textContent = s; }); }
    function setFill(id, col, alpha) {
      byId(id).forEach(function (r) { r.setAttribute('fill', col); r.setAttribute('fill-opacity', alpha); });
    }
    [].forEach.call(svg.querySelectorAll('[data-id]'), function (node) {
      if (!/^r\d+$/.test(node.getAttribute('data-id'))) node.style.pointerEvents = 'none';
    });
    var rows = data.rows, n = rows.length, showPI = data.prediction;
    var L = data.pre[0], U = data.pre[1];
    var lo0 = shown(data.reach[0]), hi0 = shown(data.reach[1]);
    var step = data.ratio ? 0.01 : (hi0 - lo0) / 400;
    function round(v) { return scaled(Math.round(shown(v) / step) * step); }
    function isPre() { return Math.abs(L - data.pre[0]) < 1e-9 && Math.abs(U - data.pre[1]) < 1e-9; }
    function fmtLimit(v) { return shown(v).toFixed(data.ratio ? 2 : Math.max(2, -Math.floor(Math.log10(step)))); }

    var bar = addControls(el,
      '<label class="ggx-range">Lower limit <input type="range" data-k="lo" min="' + lo0 + '" max="' + shown(0) +
      '" step="' + step + '" value="' + shown(L) + '" aria-label="Lower limit of little difference"> <b data-o="lo"></b></label>' +
      '<label class="ggx-range">Upper limit <input type="range" data-k="hi" min="' + shown(0) + '" max="' + hi0 +
      '" step="' + step + '" value="' + shown(U) + '" aria-label="Upper limit of little difference"> <b data-o="hi"></b></label>' +
      '<button type="button" class="ggx-nomo-reset">Back to the prespecified limits</button>' +
      (data.prediction ? '<label class="ggx-check"><input type="checkbox" checked> Prediction intervals</label>' : ''));
    var readout = document.createElement('div');
    readout.className = 'ggx-readout';
    readout.setAttribute('aria-live', 'polite');
    el.insertBefore(bar, el.firstChild);
    el.insertBefore(readout, bar.nextSibling);
    var inLo = bar.querySelector('[data-k="lo"]'), inHi = bar.querySelector('[data-k="hi"]');

    // The strips, drawn here in place of the static ones.
    byId('sb').forEach(function (r) { r.style.display = 'none'; });
    var layers = data.blocks.map(function () {
      var g = document.createElementNS(NS, 'g');
      g.setAttribute('class', 'ggx-cn-strips');
      svg.appendChild(g);
      return g;
    });
    var grids = data.blocks.map(function (B) {
      var out = [];
      for (var k = 0; k <= 200; k++) out.push(B.range[0] + k / 200 * (B.range[1] - B.range[0]));
      return out;
    });
    function drawStrip(k) {
      var B = data.blocks[k], g = layers[k], grid = grids[k];
      while (g.firstChild) g.removeChild(g.firstChild);
      rows.forEach(function (r, i) {
        var y = B.rows + i * data.strip_row;
        var bands = [[r.lo, r.hi, y, 8, false]];
        if (data.prediction) bands.push([r.plo, r.phi, y + 10, 3, true]);
        bands.forEach(function (bd) {
          var mask = function (v) { return B.which === 'upper' ? cinema.zones(bd[0], bd[1], L, v) : cinema.zones(bd[0], bd[1], v, U); };
          var j0 = 0, m0 = mask(grid[0]);
          for (var j = 1; j <= grid.length; j++) {
            var m = j < grid.length ? mask(grid[j]) : -1;
            if (m === m0) continue;
            var x0 = BX(B, grid[j0]), x1 = BX(B, grid[Math.min(j, grid.length - 1)]);
            var rect = document.createElementNS(NS, 'rect');
            rect.setAttribute('x', x0); rect.setAttribute('width', Math.max(0.5, x1 - x0));
            rect.setAttribute('y', SY(bd[2])); rect.setAttribute('height', bd[3]);
            rect.setAttribute('fill', data.ink[m0].col); rect.setAttribute('fill-opacity', data.ink[m0].alpha);
            if (bd[4]) rect.setAttribute('class', 'ggx-cn-pi');
            g.appendChild(rect);
            j0 = j; m0 = m;
          }
        });
      });
    }

    function paint() {
      bar.querySelector('[data-o="lo"]').textContent = fmtLimit(L);
      bar.querySelector('[data-o="hi"]').textContent = fmtLimit(U);
      byId('rg').forEach(function (r) { cinema.span(r, X(L), X(U)); });
      var single = 0, changed = 0, imps = [0, 0, 0], hets = [0, 0, 0];
      rows.forEach(function (r, i) {
        var k = i + 1, m = cinema.zones(r.lo, r.hi, L, U), imp = cinema.step(r.te, r.lo, r.hi, L, U);
        if (cinema.bits(m) === 1) single++;
        imps[imp]++;
        setText('rc' + k, 'CI: ' + data.words[m]);
        setFill('qc' + k, data.ink[m].col, data.ink[m].alpha);
        setText('jc' + k, 'Imprecision: ' + data.concern[imp].toLowerCase());
        setFill('mc' + k, data.levels[imp], 1);
        if (!data.prediction) return;
        var mp = cinema.zones(r.plo, r.phi, L, U);
        var het = Math.max(0, cinema.step(r.te, r.plo, r.phi, L, U) - imp);
        if (mp !== m) changed++;
        hets[het]++;
        setText('rp' + k, 'PI: ' + data.words[mp] + (mp !== m ? ' (changed)' : ''));
        setFill('qp' + k, data.ink[mp].col, data.ink[mp].alpha);
        setText('jp' + k, 'Heterogeneity: ' + data.concern[het].toLowerCase());
        setFill('mp' + k, data.levels[het], 1);
      });
      data.blocks.forEach(function (B, k) {
        var held = B.which === 'upper' ? L : U, now = B.which === 'upper' ? U : L;
        setText('st' + (k + 1), (B.which === 'upper' ? 'Upper limit from ' : 'Lower limit from ') + num(B.range[0]) + ' to ' +
          num(B.range[1]) + ', with the ' + (B.which === 'upper' ? 'lower' : 'upper') + ' limit held at ' + num(held));
        byId('sn' + (k + 1)).forEach(function (l) { cinema.line(l, BX(B, now)); });
        byId('sl' + (k + 1)).forEach(function (t) { t.setAttribute('x', BX(B, now)); t.textContent = 'now ' + num(now); });
      });
      function counts(c) {
        return c.map(function (v, k) { return v ? v + ' ' + data.concern[k].toLowerCase() : null; }).filter(Boolean).join(', ');
      }
      var pre = num(data.pre[0]) + ' to ' + num(data.pre[1]);
      readout.innerHTML = '<p>Range of little difference now: <b>' + num(L) + ' to ' + num(U) + '</b>' +
        (isPre() ? ', the prespecified limits.' : ', exploring; the prespecified limits are ' + pre + '.') + '</p>' +
        '<p>The ' + data.pct + ' CI of <b>' + single + ' of ' + n + '</b> comparisons is compatible with one reading only' +
        (single < n ? ' and <b>' + (n - single) + '</b> with more than one' : '') + '. Imprecision: ' + counts(imps) + '.' +
        (data.prediction ? ' The prediction interval changes the reading for <b>' + changed + '</b>; heterogeneity: ' +
          counts(hets) + '.' : '') + '</p>' +
        '<p class="ggx-readout-note">Move a limit with its slider, or click or tap a strip below the estimates to move that limit ' +
        'there. ' + (isPre() ? '' : 'Judgments in cinema_judge() and cinema_league() stay at the prespecified limits. ') +
        'Benefit and harm are of the first treatment against the second.</p>';
      layers.forEach(function (g) { g.style.display = ''; });
      [].forEach.call(svg.querySelectorAll('.ggx-cn-pi'), function (r) { r.style.display = showPI ? '' : 'none'; });
    }

    // Hover and click on the strips.
    var tip = cinema.tip(el);
    data.blocks.forEach(function (B, k) {
      var guide = document.createElementNS(NS, 'line');
      guide.setAttribute('y1', SY(B.rows - 4)); guide.setAttribute('y2', SY(B.rows + n * data.strip_row));
      guide.setAttribute('class', 'ggx-cn-guide');
      guide.style.display = 'none';
      svg.appendChild(guide);
      var hit = document.createElementNS(NS, 'rect');
      hit.setAttribute('x', SX(B.x0)); hit.setAttribute('width', B.x1 - B.x0);
      hit.setAttribute('y', SY(B.rows - 4)); hit.setAttribute('height', n * data.strip_row + 4);
      hit.setAttribute('class', 'ggx-cn-hit');
      svg.appendChild(hit);
      function at(ev) {
        var p = cinema.point(svg, ev);
        var v = B.range[0] + (p.x - SX(B.x0)) / (B.x1 - B.x0) * (B.range[1] - B.range[0]);
        v = round(Math.min(B.range[1], Math.max(B.range[0], v)));
        var i = Math.min(n - 1, Math.max(0, Math.floor((p.y - SY(B.rows)) / data.strip_row)));
        return { v: v, i: i };
      }
      function set(v) {
        if (B.which === 'upper') { U = v; inHi.value = shown(v); drawStrip(1); }
        else { L = v; inLo.value = shown(v); drawStrip(0); }
        paint();
      }
      var dragging = false;
      hit.addEventListener('pointermove', function (ev) {
        var s = at(ev), r = rows[s.i];
        guide.setAttribute('x1', BX(B, s.v)); guide.setAttribute('x2', BX(B, s.v));
        guide.style.display = '';
        if (dragging) { set(s.v); return; }
        var lo = B.which === 'upper' ? L : s.v, hi = B.which === 'upper' ? s.v : U;
        var m = cinema.zones(r.lo, r.hi, lo, hi);
        tip.show('<div class="ggx-tip-title">' + (B.which === 'upper' ? 'Upper' : 'Lower') + ' limit ' + num(s.v) + '</div>' +
          '<div class="ggx-tip-sub">Range ' + num(lo) + ' to ' + num(hi) + '</div>' +
          '<div class="ggx-tip-body"><b>' + escapeHtml(r.label) + '</b><br>CI: ' + escapeHtml(data.words[m]) +
          (data.prediction ? '<br>PI: ' + escapeHtml(data.words[cinema.zones(r.plo, r.phi, lo, hi)]) : '') + '</div>' +
          '<div class="ggx-tip-hint">Click to set the ' + (B.which === 'upper' ? 'upper' : 'lower') + ' limit here.</div>', ev);
      });
      hit.addEventListener('pointerleave', function () { guide.style.display = 'none'; tip.hide(); dragging = false; });
      hit.addEventListener('pointerdown', function (ev) {
        dragging = true;
        if (hit.setPointerCapture) hit.setPointerCapture(ev.pointerId);
        tip.hide();
        set(at(ev).v);
      });
      hit.addEventListener('pointerup', function () { dragging = false; });
      hit.addEventListener('pointercancel', function () { dragging = false; });
    });

    inLo.addEventListener('input', function () { L = scaled(Number(inLo.value)); drawStrip(0); paint(); });
    inHi.addEventListener('input', function () { U = scaled(Number(inHi.value)); drawStrip(1); paint(); });
    bar.querySelector('.ggx-nomo-reset').addEventListener('click', function () {
      L = data.pre[0]; U = data.pre[1];
      inLo.value = shown(L); inHi.value = shown(U);
      drawStrip(0); drawStrip(1); paint();
    });
    var check = bar.querySelector('.ggx-check input');
    if (check) {
      check.addEventListener('change', function () {
        showPI = check.checked;
        rows.forEach(function (r, i) {
          ['pi', 'rp', 'qp', 'jp', 'mp'].forEach(function (p) {
            byId(p + (i + 1)).forEach(function (node) { node.style.display = showPI ? '' : 'none'; });
          });
        });
        paint();
      });
    }
    drawStrip(0); drawStrip(1); paint();
  };

  // Where each network estimate's evidence comes from, study by study
  // (R/cinema_contribution.R). A switch colors the studies by risk of bias
  // or indirectness and regroups every bar low, moderate, high; selecting a
  // comparison, a study or part of a bar lights the bars, the matrix and the
  // small network together, and a sentence and a panel say what it shows.
  window.ggextremeCinemaContribution = function (el, data) {
    var svg = el.querySelector('svg');
    if (!svg || !data) return;
    el.classList.add('ggx-cn-contrib');
    var NS = 'http://www.w3.org/2000/svg';
    function byId(id) { return [].slice.call(svg.querySelectorAll('[data-id="' + id + '"]')); }
    function SX(v) { return data.dx + v; }
    function SY(v) { return data.dy + v; }
    function BX(v) { return SX(data.b0 + v * (data.b1 - data.b0)); }
    var C = data.comparisons, S = data.studies, n = C.length;
    var by = data.start, selC = null, selS = null, drill = null;
    function lev(j, key) { var v = S[j][key || by]; return v === undefined || v === null ? null : v; }
    function pct(v) { return (100 * v).toFixed(1) + '%'; }
    function listAnd(a) { return a.length < 2 ? a.join('') : a.slice(0, -1).join(', ') + ' and ' + a[a.length - 1]; }
    function chip(j, key) {
      var v = lev(j, key);
      if (v === null) return '<span class="ggx-cn-lv"><i class="ggx-cn-hollow"></i>not given</span>';
      return '<span class="ggx-cn-lv"><i style="background:' + data.levels[v] + '"></i>' + escapeHtml(data.short[key][v]) + '</span>';
    }
    function isDirect(i, j) { return [].concat(C[i].direct).indexOf(j) >= 0; }
    function shareOf(i, j) {
      var s = C[i].shares.filter(function (x) { return x.j === j; })[0];
      return s ? s.share : 0;
    }

    // Hatching for high judgments, drawn here in place of the static lines.
    var uid = 'ggx-cn-h' + Math.random().toString(36).slice(2, 8);
    var defs = svg.querySelector('defs') || svg.insertBefore(document.createElementNS(NS, 'defs'), svg.firstChild);
    var pat = document.createElementNS(NS, 'pattern');
    pat.setAttribute('id', uid); pat.setAttribute('patternUnits', 'userSpaceOnUse');
    pat.setAttribute('width', 4); pat.setAttribute('height', 4); pat.setAttribute('patternTransform', 'rotate(45)');
    var pl = document.createElementNS(NS, 'line');
    pl.setAttribute('x1', 0); pl.setAttribute('y1', 0); pl.setAttribute('x2', 0); pl.setAttribute('y2', 4);
    pl.setAttribute('stroke', data.levels[2]); pl.setAttribute('stroke-width', 1.3);
    pat.appendChild(pl);
    defs.appendChild(pat);
    byId('h').forEach(function (l) { l.style.display = 'none'; });
    var hatch = document.createElementNS(NS, 'g');
    hatch.setAttribute('class', 'ggx-cn-hatchlayer');
    var firstLabel = [].slice.call(svg.querySelectorAll('text[data-id^="b"]'))[0];
    if (firstLabel) firstLabel.parentNode.insertBefore(hatch, firstLabel); else svg.appendChild(hatch);
    var flowLayer = document.createElementNS(NS, 'g');
    flowLayer.setAttribute('class', 'ggx-flow');
    svg.appendChild(flowLayer);

    var segs = C.map(function (c, i) {
      return c.shares.map(function (s) {
        var nodes = byId('b' + (i + 1) + '_' + (s.j + 1));
        return {
          j: s.j, share: s.share,
          rect: nodes.filter(function (m) { return m.tagName.toLowerCase() !== 'text'; })[0],
          text: nodes.filter(function (m) { return m.tagName.toLowerCase() === 'text'; })[0]
        };
      });
    });
    var edgeLines = data.edges.map(function (e, k) { return byId('e' + (k + 1))[0]; });
    var edgeBase = edgeLines.map(function (l) { return l ? l.getAttribute('stroke-width') : 1; });
    var nodes = data.nodes.map(function (d, k) { return byId('n' + (k + 1))[0]; });
    var nodeBase = nodes.map(function (c) { return c ? c.getAttribute('stroke-width') : 1; });

    // Controls above the plot, and a sentence on what the selection shows.
    var both = data.has.rob && data.has.indirectness;
    var bar = addControls(el,
      (both ? '<span>Color studies by</span><div class="ggx-seg" role="group" aria-label="Color studies by">' +
        ['rob', 'indirectness'].map(function (k) {
          return '<button type="button" data-by="' + k + '" aria-pressed="' + (k === by) + '">' + escapeHtml(data.title[k]) + '</button>';
        }).join('') + '</div>' : '') +
      '<label class="ggx-range ggx-wrap">Comparison <select class="ggx-select"><option value="">none selected</option>' +
      C.map(function (c, i) { return '<option value="' + i + '">' + escapeHtml(c.label) + '</option>'; }).join('') +
      '</select></label><button type="button" class="ggx-nomo-reset" data-a="clear">Clear the selection</button>');
    var chips = document.createElement('div');
    chips.className = 'ggx-controls';
    chips.setAttribute('role', 'group');
    chips.setAttribute('aria-label', 'Select a study');
    var readout = document.createElement('div');
    readout.className = 'ggx-readout';
    readout.setAttribute('aria-live', 'polite');
    el.insertBefore(bar, el.firstChild);
    el.insertBefore(chips, bar.nextSibling);
    el.insertBefore(readout, chips.nextSibling);
    var select = bar.querySelector('select');

    function paintChips() {
      chips.innerHTML = '<span>Studies</span>' + S.map(function (s, j) {
        var v = lev(j);
        return '<button type="button" class="ggx-chip" data-s="' + j + '" aria-pressed="' + (selS === j) + '">' +
          '<i class="ggx-dot" style="background:' + (v === null ? 'transparent' : data.levels[v]) + '"></i>' +
          escapeHtml(s.name) + '</button>';
      }).join('');
    }
    function rectBox(r) {
      if (r.tagName.toLowerCase() === 'rect') return { y: Number(r.getAttribute('y')), h: Number(r.getAttribute('height')) };
      var b = r.getBBox();
      return { y: b.y, h: b.height };
    }

    function paint(panelToo) {
      bar.querySelectorAll('[data-by]').forEach(function (b) { b.setAttribute('aria-pressed', b.getAttribute('data-by') === by); });
      select.value = selC === null ? '' : String(selC);
      paintChips();
      byId('kt').forEach(function (t) { t.textContent = data.title[by] + ' of each study:'; });
      [0, 1, 2].forEach(function (k) {
        byId('kl' + (k + 1)).forEach(function (t) { t.textContent = data.short[by][k] + (k === 2 ? ' (hatched)' : ''); });
      });
      // Bars: grouped by judgment, low first, then by share.
      while (hatch.firstChild) hatch.removeChild(hatch.firstChild);
      segs.forEach(function (row, i) {
        var ordered = row.slice().sort(function (a, b) {
          var la = lev(a.j), lb = lev(b.j);
          return (la === null ? 9 : la) - (lb === null ? 9 : lb) || b.share - a.share;
        });
        var at = 0;
        ordered.forEach(function (sg) {
          var x0 = BX(at), x1 = BX(at + sg.share), v = lev(sg.j);
          at += sg.share;
          var dim = selS !== null && sg.j !== selS;
          var hit = selS !== null ? (sg.j === selS && (selC === null || selC === i)) :
            !!drill && drill.i === i && drill.lev === v;
          if (sg.rect) {
            cinema.span(sg.rect, x0, x1);
            sg.rect.setAttribute('fill', v === null ? '#8A8A8A' : data.levels[v]);
            sg.rect.setAttribute('stroke', hit ? '#1F1F1F' : '#FFFFFF');
            sg.rect.setAttribute('stroke-width', hit ? 2 : 1.2);
            sg.rect.style.opacity = dim ? '0.25' : '';
            if (v === 2) {
              var b = rectBox(sg.rect), o = document.createElementNS(NS, 'rect');
              o.setAttribute('x', x0); o.setAttribute('width', Math.max(0, x1 - x0));
              o.setAttribute('y', b.y); o.setAttribute('height', b.h);
              o.setAttribute('fill', 'url(#' + uid + ')');
              if (dim) o.style.opacity = '0.25';
              hatch.appendChild(o);
            }
          }
          if (sg.text) { sg.text.setAttribute('x', x0 + 4); sg.text.style.opacity = dim ? '0.25' : ''; }
        });
        byId('r' + (i + 1)).forEach(function (r) { r.setAttribute('fill-opacity', selC === i ? 1 : 0); });
        byId('mr' + (i + 1)).forEach(function (r) { r.setAttribute('fill-opacity', selC === i ? 1 : 0); });
        byId('c' + (i + 1)).forEach(function (t) { if (t.tagName.toLowerCase() === 'text') t.setAttribute('font-weight', selC === i ? 700 : 400); });
        C[i].shares.forEach(function (s) {
          var lit = (selS === null || s.j === selS) && (selC === null || selS !== null || i === selC);
          byId('m' + (i + 1) + '_' + (s.j + 1)).forEach(function (q) {
            var v = lev(s.j);
            q.setAttribute('fill', v === null ? '#8A8A8A' : data.levels[v]);
            q.setAttribute('fill-opacity', selC !== null || selS !== null ? (lit ? 1 : 0.2) : 0.85);
          });
        });
      });
      S.forEach(function (s, j) {
        var v = lev(j);
        byId('d' + (j + 1)).forEach(function (d) { d.setAttribute('fill', v === null ? '#8A8A8A' : data.levels[v]); });
        byId('mk' + (j + 1)).forEach(function (r) { r.setAttribute('fill-opacity', selS === j ? 1 : 0); });
        byId('s' + (j + 1)).forEach(function (t) { if (t.tagName.toLowerCase() === 'text') t.setAttribute('font-weight', selS === j ? 700 : 400); });
      });
      paintNet();
      say();
      if (panelToo !== false) panel();
    }

    function paintNet() {
      while (flowLayer.firstChild) flowLayer.removeChild(flowLayer.firstChild);
      edgeLines.forEach(function (l, k) {
        if (!l) return;
        l.setAttribute('stroke', '#D6DCDC');
        l.setAttribute('stroke-width', edgeBase[k]);
      });
      nodes.forEach(function (c, k) {
        if (!c) return;
        c.setAttribute('fill', '#FFFFFF');
        c.setAttribute('stroke', '#6B6B6B');
        c.setAttribute('stroke-width', nodeBase[k]);
      });
      var hint;
      if (selC !== null) {
        var c = C[selC];
        c.edges.forEach(function (e) {
          var l = edgeLines[e.e];
          if (!l || e.share < 0.005) return;
          l.setAttribute('stroke', '#22928F');
          l.setAttribute('stroke-width', (Number(edgeBase[e.e]) * (1 + 3.5 * e.share)).toFixed(2));
          var a = data.nodes.filter(function (d) { return d.t === data.edges[e.e].a; })[0];
          var b = data.nodes.filter(function (d) { return d.t === data.edges[e.e].b; })[0];
          var t = document.createElementNS(NS, 'text');
          t.setAttribute('x', SX((a.x + b.x) / 2)); t.setAttribute('y', SY((a.y + b.y) / 2) + 3.5);
          t.setAttribute('text-anchor', 'middle');
          t.setAttribute('class', 'ggx-flow-pct');
          t.textContent = (100 * e.share).toFixed(0) + '%';
          flowLayer.appendChild(t);
        });
        data.nodes.forEach(function (d, k) {
          if ((d.t === c.a || d.t === c.b) && nodes[k]) {
            nodes[k].setAttribute('stroke', '#1F1F1F');
            nodes[k].setAttribute('stroke-width', (Number(nodeBase[k]) * 2).toFixed(2));
          }
        });
        hint = 'Network: where this estimate flows from';
      } else if (selS !== null) {
        data.edges.forEach(function (e, k) {
          if ([].concat(e.studies).indexOf(selS) >= 0 && edgeLines[k]) {
            edgeLines[k].setAttribute('stroke', '#22928F');
            edgeLines[k].setAttribute('stroke-width', (Number(edgeBase[k]) * 2).toFixed(2));
          }
        });
        data.nodes.forEach(function (d, k) {
          if ([].concat(data.arms[selS]).indexOf(d.t) >= 0 && nodes[k]) {
            nodes[k].setAttribute('fill', '#22928F');
            nodes[k].setAttribute('stroke', '#22928F');
          }
        });
        hint = 'Network: what ' + S[selS].name + ' compared';
      } else {
        hint = 'Network: select a comparison or a study';
      }
      byId('nh').forEach(function (t) { t.textContent = hint; });
    }

    function shares(i) {
      var s = [0, 0, 0, 0], who = [[], [], [], []];
      C[i].shares.slice().sort(function (a, b) { return b.share - a.share; }).forEach(function (x) {
        var v = lev(x.j);
        v = v === null ? 3 : v;
        s[v] += x.share;
        who[v].push(S[x.j].name);
      });
      return { s: s, who: who };
    }
    function say() {
      var html;
      if (drill) {
        var c = C[drill.i], sh = shares(drill.i);
        html = 'In <b>' + escapeHtml(c.label) + '</b>, studies ' + escapeHtml(data.phrase[by][drill.lev]) + ' supply <b>' +
          pct(sh.s[drill.lev]) + '</b> of the estimate: ' + escapeHtml(listAnd(sh.who[drill.lev])) + '.';
      } else if (selC !== null) {
        var cc = C[selC], sc = shares(selC);
        var dir = [].concat(cc.direct).reduce(function (a, j) { return a + shareOf(selC, j); }, 0);
        var names = [].concat(cc.direct).map(function (j) { return S[j].name; });
        html = (names.length ? 'The network estimate of <b>' + escapeHtml(cc.label) + '</b> draws <b>' + pct(dir) +
          '</b> of its information from the studies that compare the pair head to head (' + escapeHtml(listAnd(names)) + ')' +
          (dir < 0.9995 ? ' and <b>' + pct(1 - dir) + '</b> through other treatments. ' : '. ') :
          'No study compares <b>' + escapeHtml(cc.a) + '</b> and <b>' + escapeHtml(cc.b) +
          '</b> head to head, so all of the network estimate comes through other treatments. ') +
          'Studies ' + escapeHtml(data.phrase[by][2]) + ' supply <b>' + pct(sc.s[2]) + '</b>, and studies ' +
          escapeHtml(data.phrase[by][1]) + ' <b>' + pct(sc.s[1]) + '</b>.';
        if (selS !== null) {
          html += ' ' + escapeHtml(S[selS].name) + ' supplies <b>' + pct(shareOf(selC, selS)) + '</b> of it' +
            (isDirect(selC, selS) ? ', head to head.' : ', through other treatments.');
        }
      } else if (selS !== null) {
        var informs = C.map(function (c, i) { return i; }).filter(function (i) { return shareOf(i, selS) > 0; });
        var direct = informs.filter(function (i) { return isDirect(i, selS); });
        var via = informs.filter(function (i) { return !isDirect(i, selS); });
        html = '<b>' + escapeHtml(S[selS].name) + '</b> informs <b>' + informs.length + ' of ' + n + '</b> network estimates. It compares ' +
          direct.length + ' of them head to head' + (via.length ? '; the other ' + via.length +
          ' receive its evidence through other treatments.' : '.');
      } else {
        var best = S.map(function (s, j) {
          var inf = C.filter(function (c, i) { return shareOf(i, j) > 0; }).length;
          var dir = C.filter(function (c, i) { return isDirect(i, j); }).length;
          return { j: j, inf: inf, dir: dir };
        }).sort(function (a, b) { return (b.inf - b.dir) - (a.inf - a.dir); })[0];
        html = 'Select a comparison, a study or part of a bar to see where the evidence comes from.' +
          (best ? ' For example, ' + escapeHtml(S[best.j].name) + ' informs <b>' + best.inf + '</b> of ' + n +
            ' network estimates although it compares only ' + best.dir + ' of these pairs head to head.' : '');
      }
      readout.innerHTML = '<p>' + html + '</p>';
    }

    function studyTable(rows, cols) {
      return '<div class="ggx-table"><table><thead><tr>' + cols.map(function (c) { return '<th scope="col">' + escapeHtml(c[0]) + '</th>'; }).join('') +
        '</tr></thead><tbody>' + rows.map(function (r) {
          return '<tr>' + cols.map(function (c) { return '<td>' + c[1](r) + '</td>'; }).join('') + '</tr>';
        }).join('') + '</tbody></table></div>';
    }
    function studyButton(j) { return '<button type="button" class="ggx-link" data-s="' + j + '">' + escapeHtml(S[j].name) + '</button>'; }
    var judgCols = [];
    if (data.has.rob) judgCols.push(['Risk of bias', function (r) { return chip(r.j, 'rob'); }]);
    if (data.has.indirectness) judgCols.push(['Indirectness', function (r) { return chip(r.j, 'indirectness'); }]);
    var panelBound = false;
    function panel() {
      var key, html;
      if (drill) {
        var c = C[drill.i], rows = c.shares.filter(function (x) { return lev(x.j) === drill.lev; })
          .sort(function (a, b) { return b.share - a.share; });
        key = 'd' + drill.i + '_' + drill.lev + by;
        html = '<div class="ggx-title">Studies ' + escapeHtml(data.phrase[by][drill.lev]) + ' in ' + escapeHtml(c.label) + '</div>' +
          '<div class="ggx-sub">' + pct(rows.reduce(function (a, r) { return a + r.share; }, 0)) + ' of this estimate</div>' +
          studyTable(rows, [['Study', function (r) { return studyButton(r.j); }], ['Share', function (r) { return pct(r.share); }],
            ['Head to head', function (r) { return isDirect(drill.i, r.j) ? 'yes' : 'no, through other treatments'; }],
            ['Reason for the judgment', function (r) { return escapeHtml(S[r.j][by + '_reason'] || 'No reason given.'); }]]);
      } else if (selC !== null) {
        var cc = C[selC], sh = shares(selC);
        key = 'c' + selC + by;
        html = '<div class="ggx-title">' + escapeHtml(cc.label) + '</div><div class="ggx-sub">Network estimate ' +
          escapeHtml(cc.estimate) + '; ' + { mixed: 'direct and indirect evidence', direct: 'direct evidence only', indirect: 'indirect evidence only' }[cc.type] + '</div>' +
          studyTable(cc.shares.slice().sort(function (a, b) { return b.share - a.share; }),
            [['Study', function (r) { return studyButton(r.j); }], ['Share', function (r) { return pct(r.share); }],
              ['Head to head', function (r) { return isDirect(selC, r.j) ? 'yes' : 'no'; }]].concat(judgCols)) +
          '<p>' + [2, 1, 0].filter(function (k) { return sh.s[k] > 0; }).map(function (k) {
            return '<button type="button" class="ggx-link" data-drill="' + k + '">List the studies ' + escapeHtml(data.phrase[by][k]) + '</button>';
          }).join(' &middot; ') + '</p><p class="ggx-note">' + escapeHtml(data.method) + '</p>';
      } else if (selS !== null) {
        var s = S[selS];
        var rowsS = C.map(function (c, i) { return { i: i, share: shareOf(i, selS) }; }).filter(function (r) { return r.share > 0; })
          .sort(function (a, b) { return b.share - a.share; });
        key = 's' + selS + by;
        html = '<div class="ggx-title">' + escapeHtml(s.name) + '</div><div class="ggx-sub">Randomized ' +
          escapeHtml(listAnd([].concat(data.arms[selS]))) + '</div>' +
          (data.has.rob ? '<p><b>Risk of bias:</b> ' + chip(selS, 'rob') + (s.rob_reason ? '. ' + escapeHtml(s.rob_reason) : '') + '</p>' : '') +
          (data.has.indirectness ? '<p><b>Indirectness:</b> ' + chip(selS, 'indirectness') +
            (s.indirectness_reason ? '. ' + escapeHtml(s.indirectness_reason) : '') + '</p>' : '') +
          '<div class="ggx-refs-head">Network estimates it informs</div>' +
          studyTable(rowsS, [['Comparison', function (r) { return escapeHtml(C[r.i].label); }], ['Share', function (r) { return pct(r.share); }],
            ['Head to head', function (r) { return isDirect(r.i, selS) ? 'yes' : 'no, through other treatments'; }]]);
      }
      var box = panelFor(hostOf(svg));
      if (!key) { box.hidden = true; return; }
      box.hidden = true;
      window.ggextremePin(svg, key, html);
      if (!panelBound) {
        panelBound = true;
        box.addEventListener('click', function (ev) {
          var t = ev.target.closest ? ev.target.closest('[data-s], [data-drill], .ggx-close') : null;
          if (!t) return;
          if (t.classList.contains('ggx-close')) { selC = null; selS = null; drill = null; paint(false); return; }
          if (t.hasAttribute('data-s')) { selS = Number(t.getAttribute('data-s')); selC = null; drill = null; }
          else { drill = { i: selC !== null ? selC : drill.i, lev: Number(t.getAttribute('data-drill')) }; selC = drill.i; selS = null; }
          paint();
        });
      }
    }

    svg.addEventListener('click', function (ev) {
      var t = ev.target.closest ? ev.target.closest('[data-id]') : null;
      var id = t && t.getAttribute('data-id');
      var m;
      if (!id) return;
      if ((m = /^c(\d+)$/.exec(id))) { var i = Number(m[1]) - 1; selC = selC === i && !drill && selS === null ? null : i; selS = null; drill = null; }
      else if ((m = /^s(\d+)$/.exec(id))) { var j = Number(m[1]) - 1; selS = selS === j && selC === null ? null : j; selC = null; drill = null; }
      else if ((m = /^b(\d+)_(\d+)$/.exec(id))) { var bi = Number(m[1]) - 1; drill = { i: bi, lev: lev(Number(m[2]) - 1) }; selC = bi; selS = null; if (drill.lev === null) drill = null; }
      else if ((m = /^m(\d+)_(\d+)$/.exec(id))) { selC = Number(m[1]) - 1; selS = Number(m[2]) - 1; drill = null; }
      else return;
      paint();
    });
    bar.addEventListener('click', function (ev) {
      var b = ev.target.closest('[data-by], [data-a]');
      if (!b) return;
      if (b.hasAttribute('data-by')) by = b.getAttribute('data-by');
      else { selC = null; selS = null; drill = null; }
      if (drill) drill = null;
      paint();
    });
    select.addEventListener('change', function () {
      selC = select.value === '' ? null : Number(select.value); selS = null; drill = null; paint();
    });
    chips.addEventListener('click', function (ev) {
      var b = ev.target.closest('[data-s]');
      if (!b) return;
      var j = Number(b.getAttribute('data-s'));
      selS = selS === j ? null : j; selC = null; drill = null;
      paint();
    });
    window.ggextremeCinemaKeys(el, 'c');
    window.ggextremeCinemaKeys(el, 's');
    paint(false);
  };

  // A network plot with one strand per study (R/cinema_network.R): a switch
  // colors the strands by risk of bias or indirectness, and a sentence
  // counts the comparisons that rest only on studies with the most serious
  // judgment, those that mix judgments and those without concerns, as
  // cinet_sentence() does in R.
  window.ggextremeCinemaNetwork = function (el, data) {
    var svg = el.querySelector('svg');
    if (!svg || !data) return;
    el.classList.add('ggx-cn-net');
    var by = data.start;
    // The static sentence under the network is written again below.
    [].forEach.call(svg.querySelectorAll('[data-id="cs"]'), function (t) { t.style.display = 'none'; });
    var strands = [].slice.call(svg.querySelectorAll('[data-id^="s"]')).filter(function (s) {
      return /^s\d+_\d+$/.test(s.getAttribute('data-id'));
    });
    var both = data.has.rob && data.has.indirectness;
    var bar = both ? addControls(el, '<span>Color studies by</span><div class="ggx-seg" role="group" aria-label="Color studies by">' +
      ['rob', 'indirectness'].map(function (k) {
        return '<button type="button" data-by="' + k + '" aria-pressed="' + (k === by) + '">' + escapeHtml(data.title[k]) + '</button>';
      }).join('') + '</div>') : null;
    var readout = document.createElement('div');
    readout.className = 'ggx-readout';
    readout.setAttribute('aria-live', 'polite');
    if (bar) { el.insertBefore(bar, el.firstChild); el.insertBefore(readout, bar.nextSibling); }
    else el.insertBefore(readout, el.firstChild);
    function listAnd(a) { return a.length < 2 ? a.join('') : a.slice(0, -1).join(', ') + ' and ' + a[a.length - 1]; }
    function names(a) { return a.length <= 3 ? listAnd(a) : a.slice(0, 3).join(', ') + ' and ' + (a.length - 3) + ' more'; }
    function paint() {
      var lv = [].concat(data[by]);
      strands.forEach(function (s) {
        var j = Number(/_(\d+)$/.exec(s.getAttribute('data-id'))[1]) - 1;
        s.setAttribute('stroke', data.levels[lv[j]]);
      });
      if (bar) bar.querySelectorAll('[data-by]').forEach(function (b) { b.setAttribute('aria-pressed', b.getAttribute('data-by') === by); });
      [].forEach.call(svg.querySelectorAll('[data-id="kt"]'), function (t) { t.textContent = data.title[by] + ' of each study:'; });
      [0, 1, 2].forEach(function (k) {
        [].forEach.call(svg.querySelectorAll('[data-id="kl' + (k + 1) + '"]'), function (t) { t.textContent = data.short[by][k]; });
      });
      var worst = [], mixed = 0, clean = 0, E = data.edges.length, ph = data.phrase[by];
      data.edges.forEach(function (e) {
        var v = [].concat(e.studies).map(function (j) { return lv[j]; });
        if (v.every(function (x) { return x === 2; })) worst.push(e.label);
        if (v.some(function (x) { return x !== v[0]; })) mixed++;
        if (v.every(function (x) { return x === 0; })) clean++;
      });
      readout.innerHTML = '<p>Judged by ' + escapeHtml(data.title[by].toLowerCase()) + ', ' +
        (worst.length ? '<b>' + worst.length + '</b> of ' + E + ' direct comparisons ' + (worst.length === 1 ? 'rests' : 'rest') +
          ' only on studies ' + escapeHtml(ph[2]) + ' (' + escapeHtml(names(worst)) + '), ' :
          'no direct comparison rests only on studies ' + escapeHtml(ph[2]) + ', ') +
        '<b>' + mixed + '</b> ' + (mixed === 1 ? 'mixes' : 'mix') + ' studies with different judgments, which one averaged color would hide, and <b>' +
        clean + '</b> ' + (clean === 1 ? 'rests' : 'rest') + ' only on studies ' + escapeHtml(ph[0]) + '.</p>' +
        '<p class="ggx-readout-note">Each strand is one study. Hover over a strand for its study; click a line or a treatment, or press Enter on it, for the studies behind it and the reasons for their judgments.</p>';
    }
    if (bar) bar.addEventListener('click', function (ev) {
      var b = ev.target.closest('[data-by]');
      if (!b) return;
      by = b.getAttribute('data-by');
      paint();
    });
    paint();
  };

  // Restricted mean survival in a Kaplan-Meier plot: a slider moves the
  // horizon, and the shaded areas, the table under the risk table and the
  // sentence below it follow. The prespecified horizon stays marked.
  window.ggextremeRmst = function (el, data) {
    var svg = el.querySelector('svg');
    if (!svg || !data) return;
    var NS = 'http://www.w3.org/2000/svg';
    function X(t) { return data.dx + data.x0 + Math.min(t, data.tmax) / data.tmax * (data.x1 - data.x0); }
    function Y(p) { return data.dy + data.bottom - p * (data.bottom - data.top); }
    function prob(v) { return data.type === 'survival' ? v : 1 - v; }
    var digits = data.limit >= 100 ? 0 : data.limit >= 10 ? 1 : 2;
    function f(v) { return Number(v).toFixed(digits); }
    function ft(v) { return Math.abs(v - Math.round(v)) < 1e-9 ? String(Math.round(v)) : f(v); }
    function byId(id) { return [].slice.call(svg.querySelectorAll('[data-id="' + id + '"]')); }
    var areas = data.arms.map(function (_, a) { return byId('rma' + (a + 1))[0]; });
    var line = byId('rmt')[0];
    // Our own shapes take the place of the static ones, which are hidden.
    var shapes = areas.map(function (old, a) {
      var p = document.createElementNS(NS, 'polygon');
      p.setAttribute('fill', a ? data.colors[a] : '#6B6B6B');
      p.setAttribute('fill-opacity', a ? '0.3' : '0.1');
      p.setAttribute('class', 'ggx-rmst-area');
      if (old) { old.parentNode.insertBefore(p, old); old.style.display = 'none'; }
      return p;
    });
    var tauLine = document.createElementNS(NS, 'line');
    tauLine.setAttribute('class', 'ggx-rmst-tau');
    if (line) { line.parentNode.insertBefore(tauLine, line); line.style.display = 'none'; }
    function setText(id, v) { byId(id).forEach(function (t) { t.textContent = v; }); }
    function steps(cv, tau) {
      var pts = [[0, 1]];
      var last = 1;
      for (var i = 0; i < cv.t.length && cv.t[i] <= tau; i++) {
        pts.push([cv.t[i], last]);
        pts.push([cv.t[i], cv.s[i]]);
        last = cv.s[i];
      }
      pts.push([tau, last]);
      return pts;
    }
    var i = data.start;
    var readout = document.createElement('div');
    readout.className = 'ggx-readout';
    readout.setAttribute('aria-live', 'polite');
    var bar = addControls(el, '<label class="ggx-range">Horizon \u03c4 <input type="range" min="0" max="' +
      (data.taus.length - 1) + '" step="1" value="' + i + '" aria-label="Horizon"> <b></b></label>' +
      '<button type="button" class="ggx-nomo-reset">' + (data.prespecified ? 'Prespecified \u03c4' : 'Default \u03c4') + '</button>');
    el.insertBefore(readout, bar.nextSibling);
    var input = bar.querySelector('input');
    var out = bar.querySelector('b');
    function draw() {
      var tau = data.taus[i];
      // The reference's area under its curve; the others' areas between
      // their curve and the reference's, which are the differences.
      var ref = steps(data.curves[0], tau);
      shapes.forEach(function (p, a) {
        var pts = a ? steps(data.curves[a], tau).concat(ref.slice().reverse()) : ref.concat([[tau, 0], [0, 0]]);
        p.setAttribute('points', pts.map(function (q) {
          return X(q[0]).toFixed(1) + ',' + Y(prob(q[1])).toFixed(1);
        }).join(' '));
      });
      tauLine.setAttribute('x1', X(tau)); tauLine.setAttribute('x2', X(tau));
      tauLine.setAttribute('y1', Y(1)); tauLine.setAttribute('y2', Y(0));
      byId('rml').forEach(function (t) { t.setAttribute('x', X(tau) + 4); });
      var z = 1.959964;
      var what = data.type === 'survival' ? 'Restricted mean survival time' : 'Restricted mean time free of the event';
      setText('rml', '\u03c4 = ' + ft(tau));
      setText('rmh', what + ' up to \u03c4 = ' + ft(tau));
      data.arms.forEach(function (_, a) {
        var m = data.m[i][a], se = data.se[i][a];
        setText('rmv' + (a + 1), f(m) + ' (' + f(m - z * se) + ' to ' + f(m + z * se) + ')');
        if (a > 0) {
          var d = data.d[i][a], ds = data.dse[i][a];
          setText('rmd' + (a + 1), f(d) + ' (' + f(d - z * ds) + ' to ' + f(d + z * ds) + ')');
        }
      });
      out.textContent = ft(tau);
      input.setAttribute('aria-valuetext', ft(tau));
      var noun = data.type === 'survival' ? 'mean time alive' : 'mean time free of the event';
      var lines = data.arms.slice(1).map(function (arm, k) {
        var a = k + 1, d = data.d[i][a], ds = data.dse[i][a];
        var lo = d - z * ds, hi = d + z * ds;
        var verdict = lo > 0 || hi < 0 ? '' : '; the interval includes no difference';
        return 'Up to \u03c4 = ' + ft(tau) + ', the ' + noun + ' was <b>' + f(data.m[i][a]) + '</b> with ' +
          escapeHtml(arm) + ' and <b>' + f(data.m[i][0]) + '</b> with ' + escapeHtml(data.arms[0]) +
          ': a difference of <b>' + f(d) + '</b> (95% CI ' + f(lo) + ' to ' + f(hi) + ')' + verdict + '.';
      });
      var off = Math.abs(tau - data.taus[data.start]) > 1e-9;
      readout.innerHTML = lines.map(function (l) { return '<p>' + l + '</p>'; }).join('') +
        '<p class="ggx-readout-note">Times are in the units of the time axis (' + escapeHtml(data.xlab) + ').' +
        (off ? ' This horizon is exploratory; the ' + (data.prespecified ? 'prespecified' : 'default') +
          ' horizon is \u03c4 = ' + ft(data.taus[data.start]) + '.' : '') +
        ' The difference assumes independent groups.</p>';
    }
    input.addEventListener('input', function () { i = +input.value; draw(); });
    bar.querySelector('button').addEventListener('click', function () { i = data.start; input.value = i; draw(); });
    draw();
  };

  // A nomogram. The widget's own plot is an empty canvas; this draws the
  // axes, a handle for every predictor and the prediction, and recomputes
  // the prediction and its interval from the model whenever a handle moves.
  window.ggextremeNomogram = function (el, data) {
    var svg = el.querySelector('svg');
    if (!svg || !data) return;
    el.classList.add('ggx-nomogram');
    if (window.getComputedStyle(el).position === 'static') el.style.position = 'relative';
    var NS = 'http://www.w3.org/2000/svg';
    function mk(tag, attrs, parent) {
      var node = document.createElementNS(NS, tag);
      for (var k in attrs) node.setAttribute(k, attrs[k]);
      if (parent) parent.appendChild(node);
      return node;
    }
    function label(parent, x, y, s, cls, anchor) {
      var t = mk('text', { x: x, y: y, 'class': cls, 'text-anchor': anchor || 'start' }, parent);
      t.textContent = s;
      return t;
    }
    var vars = data.vars;
    var blocks = data.blocks;
    var eqi = data.eq_start;
    var stratum = 0;
    var X0 = data.dx + data.x0;
    var W = data.axis_w;
    function Y(v) { return v + data.dy; }
    function X(pts) { return X0 + pts / 100 * W; }
    function eq() { return data.eqs[eqi]; }
    function XT(t) { return X0 + t / eq().max_pts * W; }
    var root = mk('g', { 'class': 'ggx-nomo' }, svg);
    var z = 1.959964;

    // Current positions: a value for a numeric predictor, a level index
    // otherwise. The starting values are the references points count from.
    var start = vars.map(function (v) { return v.type === 'cont' ? v.values[v.start] : v.start; });
    var pos = start.slice();

    // A cluster's design columns and offset at the given positions, with
    // numeric predictors interpolated between grid points.
    function blockAt(b, at) {
      var parts = b.vars.map(function (vi) {
        var v = vars[vi];
        var p = at[vi];
        if (v.type !== 'cont') return [[p, 1]];
        var g = v.values;
        var n = g.length;
        if (n < 2) return [[0, 1]];
        var t = (Math.min(Math.max(p, g[0]), g[n - 1]) - g[0]) / (g[1] - g[0]);
        var i0 = Math.min(Math.floor(t + 1e-9), n - 2);
        var f = Math.max(0, t - i0);
        return f < 1e-9 ? [[i0, 1]] : [[i0, 1 - f], [i0 + 1, f]];
      });
      var ncol = b.cols.length;
      var x = new Array(ncol).fill(0);
      var off = 0;
      var stride = [];
      var s = 1;
      b.dims.forEach(function (d) { stride.push(s); s *= d; });
      (function walk(j, row, w) {
        if (j === parts.length) {
          var xr = b.X[row];
          for (var c = 0; c < ncol; c++) x[c] += w * xr[c];
          off += w * b.off[row];
          return;
        }
        parts[j].forEach(function (p) { walk(j + 1, row + p[0] * stride[j], w * p[1]); });
      })(0, 0, 1);
      return { x: x, off: off };
    }
    function contrib(b, at) {
      var r = blockAt(b, at);
      var beta = eq().beta;
      var f = r.off;
      for (var c = 0; c < b.cols.length; c++) f += r.x[c] * beta[b.cols[c]];
      return f;
    }
    // A row's points: its predictor's step from the reference, with the
    // earlier predictors of its cluster at their current values.
    function rowDelta(r, at) {
      var b = blocks[r.block];
      var now = start.slice();
      var before = start.slice();
      b.vars.forEach(function (vi, j) {
        if (j <= r.k) now[vi] = at[vi];
        if (j < r.k) before[vi] = at[vi];
      });
      return contrib(b, now) - contrib(b, before);
    }
    function rowPoints(r, at) { return (rowDelta(r, at) - r.lo) / eq().scale * 100; }
    function nearestIndex(v, value) {
      var g = v.values;
      var best = 0;
      for (var i = 1; i < g.length; i++) if (Math.abs(g[i] - value) < Math.abs(g[best] - value)) best = i;
      return best;
    }
    function condIndex(r) {
      var b = blocks[r.block];
      var idx = 0;
      var s = 1;
      for (var j = 0; j < r.k; j++) {
        var vi = b.vars[j];
        var v = vars[vi];
        idx += (v.type === 'cont' ? nearestIndex(v, pos[vi]) : pos[vi]) * s;
        s *= b.dims[j];
      }
      return idx;
    }
    function valueText(v, p) {
      if (v.type === 'cont') {
        return Number(p).toLocaleString('en-US', { minimumFractionDigits: v.digits, maximumFractionDigits: v.digits });
      }
      return String(v.values[p]);
    }

    // Links and distributions, as R's make.link() and survreg() define them.
    function pnorm(x) {
      var t = 1 / (1 + 0.2316419 * Math.abs(x));
      var d = 0.3989422804014327 * Math.exp(-x * x / 2);
      var p = d * t * (0.319381530 + t * (-0.356563782 + t * (1.781477937 + t * (-1.821255978 + t * 1.330274429))));
      return x >= 0 ? 1 - p : p;
    }
    var inverse = {
      identity: function (e) { return e; },
      log: Math.exp,
      logit: function (e) { return 1 / (1 + Math.exp(-e)); },
      probit: pnorm,
      cloglog: function (e) { return 1 - Math.exp(-Math.exp(e)); },
      loglog: function (e) { return Math.exp(-Math.exp(-e)); },
      cauchit: function (e) { return 0.5 + Math.atan(e) / Math.PI; },
      inverse: function (e) { return 1 / e; },
      '1/mu^2': function (e) { return 1 / Math.sqrt(e); },
      sqrt: function (e) { return e * e; }
    };
    var cdf = { extreme: inverse.cloglog, logistic: inverse.logit, gaussian: pnorm };
    function quad(g, V) {
      var s = 0;
      for (var i = 0; i < g.length; i++) {
        if (!g[i]) continue;
        for (var j = 0; j < g.length; j++) s += g[i] * V[i][j] * g[j];
      }
      return Math.sqrt(Math.max(0, s));
    }

    // The design row, the linear predictor and every prediction.
    function predict() {
      var x = data['const'].slice();
      var off = data.const_off;
      blocks.forEach(function (b) {
        var r = blockAt(b, pos);
        b.cols.forEach(function (c, j) { x[c] += r.x[j]; });
        off += r.off;
      });
      var beta = eq().beta;
      var lp = off;
      for (var i = 0; i < x.length; i++) lp += x[i] * beta[i];
      var out = [];
      var kind = data.kind;
      var V = data.V;
      if (kind === 'linear' || kind === 'link') {
        var se = quad(x, V);
        var q = kind === 'linear' ? data.q : z;
        var f = kind === 'linear' ? inverse.identity : inverse[data.link] || inverse.identity;
        var a = f(lp - q * se);
        var c = f(lp + q * se);
        out.push({ label: data.labels[0], est: f(lp), lo: Math.min(a, c), hi: Math.max(a, c) });
        if (kind === 'linear' && data.sigma) {
          var s2 = Math.sqrt(se * se + data.sigma * data.sigma);
          out.push({ label: 'Prediction interval', est: lp, lo: lp - q * s2, hi: lp + q * s2, pi: true });
        }
      } else if (kind === 'cox') {
        var xc = x.map(function (v, k) { return v - data.ctr[k]; });
        var eta = 0;
        for (var k = 0; k < xc.length; k++) eta += xc[k] * beta[k];
        var r = Math.exp(eta);
        data.strata[stratum].at.forEach(function (at, j) {
          var d = xc.map(function (v, k) { return v * at.H0 - at.A[k]; });
          var dVd = quad(d, V);
          var seH = r * Math.sqrt(Math.max(0, at.T1 + dVd * dVd));
          var S = Math.exp(-at.H0 * r);
          out.push({ label: data.labels[j], est: S, lo: S * Math.exp(-z * seH), hi: Math.min(1, S * Math.exp(z * seH)), prob: true });
        });
      } else if (kind === 'aft') {
        var A = data.aft;
        var F = cdf[A.base];
        var tr = A.trans === 'log' ? Math.log : inverse.identity;
        var un = A.trans === 'log' ? Math.exp : inverse.identity;
        var qm = lp + A.scale * A.q50;
        var sq = quad(A.has_scale ? x.concat([A.scale * A.q50]) : x, V);
        out.push({ label: data.labels[0], est: un(qm), lo: un(qm - z * sq), hi: un(qm + z * sq) });
        A.times.forEach(function (t, j) {
          var u = (tr(t) - lp) / A.scale;
          var gx = x.map(function (v) { return -v / A.scale; });
          var su = quad(A.has_scale ? gx.concat([-u]) : gx, V);
          out.push({ label: data.labels[j + 1], est: 1 - F(u), lo: 1 - F(u + z * su), hi: 1 - F(u - z * su), prob: true });
        });
      } else if (kind === 'ordinal') {
        var O = data.ordinal;
        var G = inverse[O.link];
        var cum = [1];
        O.zeta.forEach(function (zt, j) {
          var g = x.map(function (v) { return -v; }).concat(O.zeta.map(function (_, m) { return m === j ? 1 : 0; }));
          var su = quad(g, V);
          var u = zt - lp;
          out.push({ label: data.labels[j], est: 1 - G(u), lo: 1 - G(u + z * su), hi: 1 - G(u - z * su), prob: true });
          cum.push(1 - G(u));
        });
        cum.push(0);
        out.categories = O.levels.map(function (lv, m) { return { label: lv, p: cum[m] - cum[m + 1] }; });
      } else {
        var lps = data.eqs.map(function (e2) {
          var s = 0;
          for (var i = 0; i < x.length; i++) s += x[i] * e2.beta[i];
          return s;
        });
        var den = 1 + lps.reduce(function (a, v) { return a + Math.exp(v); }, 0);
        out.push({ label: data.labels[eqi], est: Math.exp(lp) });
        out.categories = data.levels.map(function (lv, m) { return { label: lv, p: (m ? Math.exp(lps[m - 1]) : 1) / den }; });
      }
      return { lp: lp, total: (lp - eq().base0) / eq().scale * 100, out: out };
    }

    function fmt(v, prob) {
      if (!isFinite(v)) return 'not estimable';
      if (prob) {
        var p = 100 * v;
        return (p < 10 ? p.toFixed(1) : p.toFixed(0)) + '%';
      }
      var a = Math.abs(v);
      var d = a >= 100 ? 0 : a >= 10 ? 1 : 2;
      return v.toLocaleString('en-US', { minimumFractionDigits: d, maximumFractionDigits: d });
    }

    // Floating hover card.
    var tip = document.createElement('div');
    tip.className = 'ggx-float-tip';
    tip.hidden = true;
    el.appendChild(tip);
    function showTip(html, ev) {
      tip.innerHTML = html;
      tip.hidden = false;
      var box = el.getBoundingClientRect();
      var x = ev.clientX - box.left + 14;
      var y = ev.clientY - box.top + 14;
      if (x + tip.offsetWidth > box.width - 4) x = ev.clientX - box.left - tip.offsetWidth - 14;
      tip.style.left = Math.max(4, x) + 'px';
      tip.style.top = Math.max(4, y) + 'px';
    }
    function hideTip() { tip.hidden = true; }

    function svgPoint(ev) {
      var p = svg.createSVGPoint();
      p.x = ev.clientX;
      p.y = ev.clientY;
      return p.matrixTransform(svg.getScreenCTM().inverse());
    }

    // Drawing. The static layers are redrawn when the equation or stratum
    // changes; each row redraws when its condition changes.
    var layers = {};
    function drawAxis(parent, y, lo, hi, ticks, below) {
      mk('line', { x1: lo, x2: hi, y1: y, y2: y, 'class': 'ggx-nomo-axis' }, parent);
      ticks.forEach(function (t) {
        mk('line', { x1: t.x, x2: t.x, y1: y, y2: below ? y + 4 : y - 4, 'class': 'ggx-nomo-axis' }, parent);
        label(parent, t.x, below ? y + 13 + (t.line || 0) * data.stagger : y - 8, t.label, 'ggx-nomo-tick', 'middle');
      });
    }
    function drawFrame() {
      if (layers.frame) layers.frame.remove();
      var g = mk('g', {}, root);
      layers.frame = g;
      root.insertBefore(g, root.firstChild);
      var ticks = [];
      for (var v = 0; v <= 100; v += 10) ticks.push({ x: X(v), label: String(v) });
      drawAxis(g, Y(data.points_y), X(0), X(100), ticks, false);
      label(g, X0 - 12, Y(data.points_y) + 4, 'Points', 'ggx-nomo-head', 'end');
      var e = eq();
      drawAxis(g, Y(data.total_y), XT(0), XT(e.max_pts), e.total.map(function (t) { return { x: XT(t), label: String(t) }; }), true);
      label(g, X0 - 12, Y(data.total_y) + 4, 'Total points', 'ggx-nomo-head', 'end');
      e.outputs.forEach(function (o) {
        var tk = o.strata[Math.min(stratum, o.strata.length - 1)].map(function (t) { return { x: XT(t[1]), label: t[0] }; });
        var xs = tk.map(function (t) { return t.x; });
        drawAxis(g, Y(o.y), xs.length ? Math.min.apply(null, xs) : XT(0), xs.length ? Math.max.apply(null, xs) : XT(e.max_pts), tk, true);
        label(g, X0 - 12, Y(o.y) + 4, o.label, 'ggx-nomo-head', 'end');
      });
    }

    var rowState = [];
    function drawRow(i) {
      var r = eq().rows[i];
      var st = rowState[i] || (rowState[i] = {});
      var ci = condIndex(r);
      if (st.g && st.eq === eqi && st.cond === ci) return st;
      var v = vars[r['var']];
      var cd = r.conds[ci];
      // A redrawn row takes its old place, so the tab order stays the same.
      var g = mk('g', { 'class': 'ggx-nomo-row' });
      if (st.g) { rowsG.replaceChild(g, st.g); } else rowsG.appendChild(g);
      var y = Y(r.y);
      var height = (cd.runs.length - 1) * data.run_gap + 30;
      st.hit = mk('rect', { x: data.dx, y: y - 16, width: X(100) - data.dx + 50, height: height + 4, 'class': 'ggx-nomo-hit' }, g);
      label(g, X0 - 12, y + 4, v.label, 'ggx-nomo-label', 'end');
      if (r.conditional) {
        var b = blocks[r.block];
        var given = b.vars.slice(0, r.k).map(function (vi) { return vars[vi].label + ' = ' + valueText(vars[vi], vars[vi].type === 'cont' ? pos[vi] : pos[vi]); });
        label(g, X0 - 12, y + 16, 'if ' + given.join(', '), 'ggx-nomo-tick', 'end');
      }
      cd.runs.forEach(function (run, ri) {
        var yr = y + ri * data.run_gap;
        var tk = cd.ticks.filter(function (t) { return t[2] === ri; }).map(function (t) { return { x: X(t[1]), label: t[0], line: t[3] }; });
        drawAxis(g, yr, X(run[0]), X(run[1]), tk, true);
      });
      st.guide = mk('line', { 'class': 'ggx-nomo-guide' }, g);
      st.handle = mk('g', { 'class': 'ggx-nomo-handle', tabindex: 0, role: 'slider', 'aria-label': v.label }, g);
      mk('circle', { r: 6.5 }, st.handle);
      st.value = label(st.handle, 0, -11, '', 'ggx-nomo-value', 'middle');
      st.pts = label(g, X(100) + 12, y + 4, '', 'ggx-nomo-pts', 'start');
      st.g = g;
      st.eq = eqi;
      st.cond = ci;
      st.cd = cd;
      bindRow(i, st, r, v);
      return st;
    }

    // Where each grid value of a numeric row sits, for dragging along a
    // folded axis.
    function rowGrid(r, v, cd) {
      var at = pos.slice();
      return v.values.map(function (val, gi) {
        at[r['var']] = v.type === 'cont' ? val : gi;
        var run = cd.seg_run ? cd.seg_run[Math.min(gi, cd.seg_run.length - 1)] : 0;
        return { x: X(rowPoints(r, at)), y: Y(r.y) + run * data.run_gap, value: v.type === 'cont' ? val : gi };
      });
    }
    function bindRow(i, st, r, v) {
      function pick(ev) {
        var p = svgPoint(ev);
        var grid = rowGrid(r, v, st.cd);
        var best = grid[0];
        var bd = Infinity;
        grid.forEach(function (q) {
          var d = (q.x - p.x) * (q.x - p.x) + (q.y - p.y) * (q.y - p.y) * 4;
          if (d < bd) { bd = d; best = q; }
        });
        set(r['var'], best.value);
      }
      var dragging = false;
      st.handle.addEventListener('pointerdown', function (ev) {
        dragging = true;
        st.handle.setPointerCapture(ev.pointerId);
        ev.preventDefault();
      });
      st.handle.addEventListener('pointermove', function (ev) { if (dragging) pick(ev); });
      st.handle.addEventListener('pointerup', function () { dragging = false; });
      st.hit.addEventListener('click', pick);
      st.handle.addEventListener('keydown', function (ev) {
        var up = ev.key === 'ArrowRight' || ev.key === 'ArrowUp';
        var down = ev.key === 'ArrowLeft' || ev.key === 'ArrowDown';
        if (!up && !down) return;
        ev.preventDefault();
        var vi = r['var'];
        if (v.type === 'cont') set(vi, Math.min(v.hi, Math.max(v.lo, Math.round((pos[vi] + (up ? v.step : -v.step)) / v.step) * v.step)));
        else set(vi, Math.min(v.values.length - 1, Math.max(0, pos[vi] + (up ? 1 : -1))));
      });
      st.g.addEventListener('pointermove', function (ev) {
        if (dragging) return;
        var obs = v.observed ? '<div class="ggx-tip-row"><span>Observed range</span><span>' +
          valueText({ type: 'cont', digits: v.digits || 0 }, v.observed[0]) + ' to ' +
          valueText({ type: 'cont', digits: v.digits || 0 }, v.observed[1]) + '</span></div>' : '';
        showTip('<div class="ggx-tip-title">' + escapeHtml(v.label) + '</div>' +
          '<div class="ggx-tip-row"><span>Now</span><span>' + escapeHtml(valueText(v, pos[r['var']])) + '</span></div>' +
          '<div class="ggx-tip-row"><span>Points</span><span>' + rowPoints(r, pos).toFixed(0) + '</span></div>' + obs +
          '<div class="ggx-tip-hint">Drag the handle, click the axis or use the arrow keys.</div>', ev);
      });
      st.g.addEventListener('pointerleave', hideTip);
    }

    var rowsG = mk('g', {}, root);
    var marks = mk('g', {}, root);
    var readout = document.createElement('div');
    readout.className = 'ggx-nomo-readout';
    readout.setAttribute('aria-live', 'polite');
    var section = el.querySelector(':scope > .ggx-details');
    if (section) el.insertBefore(readout, section); else el.appendChild(readout);

    function update() {
      var rows = eq().rows;
      rows.forEach(function (r, i) {
        var st = drawRow(i);
        var v = vars[r['var']];
        var pts = rowPoints(r, pos);
        var run = 0;
        if (st.cd.seg_run && v.type === 'cont') {
          var gi = nearestIndex(v, pos[r['var']]);
          run = st.cd.seg_run[Math.min(gi, st.cd.seg_run.length - 1)];
        }
        var hx = X(pts);
        var hy = Y(r.y) + run * data.run_gap;
        st.handle.setAttribute('transform', 'translate(' + hx + ' ' + hy + ')');
        st.handle.setAttribute('aria-valuetext', valueText(v, pos[r['var']]) + ', ' + pts.toFixed(0) + ' points');
        st.value.textContent = v.type === 'cont' ? valueText(v, pos[r['var']]) : '';
        st.guide.setAttribute('x1', hx);
        st.guide.setAttribute('x2', hx);
        st.guide.setAttribute('y1', Y(data.points_y));
        st.guide.setAttribute('y2', hy);
        st.pts.textContent = pts.toFixed(0) + ' pts';
        st.handle.querySelector('circle').setAttribute('class', '');
      });
      var res = predict();
      marks.innerHTML = '';
      var xt = XT(Math.min(eq().max_pts, Math.max(0, res.total)));
      var last = eq().outputs.length ? eq().outputs[eq().outputs.length - 1].y : data.total_y;
      mk('line', { x1: xt, x2: xt, y1: Y(data.total_y), y2: Y(last), 'class': 'ggx-nomo-drop' }, marks);
      [data.total_y].concat(eq().outputs.map(function (o) { return o.y; })).forEach(function (y) {
        mk('circle', { cx: xt, cy: Y(y), r: 5.5, 'class': 'ggx-nomo-dot' }, marks);
      });
      var lines = res.out.map(function (o, k) {
        var axis = eq().outputs[k];
        var prob = o.prob || (!o.pi && axis && axis.prob);
        var ci = o.lo === undefined ? '' : '<span class="ggx-nomo-ci">' + (o.pi ? '' : '95% CI ') + fmt(o.lo, prob) + ' to ' + fmt(o.hi, prob) + '</span>';
        if (o.pi) return '<div class="ggx-nomo-line"><span>Prediction interval for a new patient</span>' + ci + '</div>';
        return '<div class="ggx-nomo-line"><span>' + escapeHtml(o.label) + '</span><b>' + fmt(o.est, prob) + '</b>' + ci + '</div>';
      }).join('');
      var cats = res.out.categories ? '<div class="ggx-nomo-cats">' + res.out.categories.map(function (c, m) {
        return '<div class="ggx-nomo-cat" style="flex:' + Math.max(c.p, 0.0001) + '" title="' + escapeHtml(c.label) + '"><i style="opacity:' + (0.25 + 0.75 * (m + 1) / res.out.categories.length) + '"></i></div>';
      }).join('') + '</div><div class="ggx-nomo-catlabels">' + res.out.categories.map(function (c) {
        return '<span>' + escapeHtml(c.label) + ' <b>' + fmt(c.p, true) + '</b></span>';
      }).join('') + '</div>' : '';
      readout.querySelector('.ggx-nomo-lines').innerHTML = lines + cats +
        '<div class="ggx-nomo-total">' + res.total.toFixed(0) + ' total points</div>';
    }
    function set(vi, value) {
      pos[vi] = value;
      update();
    }

    readout.innerHTML = '<div class="ggx-nomo-lines"></div>';
    var buttons = [];
    if (data.kind === 'multinomial' && data.eqs.length > 1) {
      buttons.push('<span>Scale on</span><div class="ggx-seg" data-group="eq">' + data.eqs.map(function (e, k) {
        return '<button type="button" aria-pressed="' + (k === eqi) + '" data-k="' + k + '">' + escapeHtml(e.label) + '</button>';
      }).join('') + '</div>');
    }
    if (data.kind === 'cox' && data.strata.length > 1) {
      buttons.push('<span>Stratum</span><div class="ggx-seg" data-group="stratum">' + data.strata.map(function (s, k) {
        return '<button type="button" aria-pressed="' + (k === stratum) + '" data-k="' + k + '">' + escapeHtml(s.label) + '</button>';
      }).join('') + '</div>');
    }
    buttons.push('<button type="button" class="ggx-nomo-reset">Reset</button>');
    var bar = addControls(el, buttons.join(''));
    [].forEach.call(bar.querySelectorAll('.ggx-seg'), function (seg) {
      [].forEach.call(seg.querySelectorAll('button'), function (b) {
        b.addEventListener('click', function () {
          [].forEach.call(seg.querySelectorAll('button'), function (o) { o.setAttribute('aria-pressed', o === b); });
          if (seg.getAttribute('data-group') === 'eq') {
            eqi = +b.getAttribute('data-k');
            rowState.forEach(function (st) { if (st.g) st.g.remove(); });
            rowState = [];
          } else stratum = +b.getAttribute('data-k');
          drawFrame();
          update();
        });
      });
    });
    bar.querySelector('.ggx-nomo-reset').addEventListener('click', function () {
      pos = start.slice();
      update();
    });
    el.insertBefore(readout, bar);
    drawFrame();
    update();
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

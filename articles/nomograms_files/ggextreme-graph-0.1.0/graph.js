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

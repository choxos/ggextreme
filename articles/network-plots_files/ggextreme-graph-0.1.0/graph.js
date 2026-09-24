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

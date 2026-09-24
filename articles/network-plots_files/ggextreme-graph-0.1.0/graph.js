// Opens the details panel for a node or edge when it is clicked.
//
// The panel sits directly after the widget, one per widget, and is created
// on the first click. Clicking the same item again, or the close button,
// hides it; clicking another item replaces its contents.
(function () {
  function hostOf(el) {
    return el.closest('.html-widget') || el.closest('svg').parentNode;
  }

  function panelFor(host) {
    var next = host.nextElementSibling;
    if (next && next.classList.contains('ggx-panel')) return next;
    var panel = document.createElement('div');
    panel.className = 'ggx-panel';
    panel.setAttribute('role', 'region');
    panel.setAttribute('aria-live', 'polite');
    panel.hidden = true;
    host.parentNode.insertBefore(panel, host.nextSibling);
    return panel;
  }

  // Cap the widget at the diagram's natural width, given by its viewBox in
  // points, and let its height follow the width.
  window.ggextremeFit = function (el) {
    var svg = el.querySelector('svg');
    if (!svg || !svg.viewBox || !svg.viewBox.baseVal) return;
    el.style.maxWidth = (svg.viewBox.baseVal.width * 4 / 3) + 'px';
    el.style.height = 'auto';
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
    var box = window.getComputedStyle(host);
    // A panel with a table gets room for it even under a narrow plot.
    var cap = parseFloat(host.style.maxWidth) || 0;
    panel.style.maxWidth = Math.max(cap, 760) + 'px';
    panel.style.marginLeft = box.marginLeft;
    panel.style.marginRight = box.marginRight;
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

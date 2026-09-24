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

  window.ggextremePin = function (el, key, html) {
    var host = hostOf(el);
    var panel = panelFor(host);
    var box = window.getComputedStyle(host);
    panel.style.maxWidth = host.style.maxWidth;
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

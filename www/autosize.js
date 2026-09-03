// Auto-sizing text areas -----------------------------------------------------
//
// Fields open at the height of their placeholder rather than a fixed number of
// rows, so a page of empty boxes stays compact, then each grows as it is filled
// in. Nobody has to scroll inside a text box, and nobody meets a wall of them.

(function () {
  "use strict";

  var SELECTOR = ".ftp-main textarea";

  // Measure against the placeholder when the field is empty, so an empty box is
  // exactly big enough to show the example rather than clipping it. The value is
  // restored synchronously and no events are dispatched, so Shiny never sees it.
  function fit(el) {
    var borrowed = false;
    if (!el.value && el.placeholder) {
      el.value = el.placeholder;
      borrowed = true;
    }

    el.style.height = "auto";
    el.style.height = (el.scrollHeight + 2) + "px";

    if (borrowed) el.value = "";
  }

  function fitAll(root) {
    var scope = root && root.querySelectorAll ? root : document;
    Array.prototype.forEach.call(scope.querySelectorAll(SELECTOR), fit);
  }

  // Typing, pasting, and Shiny writing a value back into a field.
  document.addEventListener("input", function (e) {
    if (e.target.matches && e.target.matches(SELECTOR)) fit(e.target);
  });

  // Trial blocks and questions are added after load, so watch for new fields
  // rather than sizing once at startup.
  var observer = new MutationObserver(function (mutations) {
    mutations.forEach(function (m) {
      Array.prototype.forEach.call(m.addedNodes, function (node) {
        if (node.nodeType !== 1) return;
        if (node.matches && node.matches(SELECTOR)) fit(node);
        else fitAll(node);
      });
    });
  });

  function start() {
    fitAll(document);
    observer.observe(document.body, { childList: true, subtree: true });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", start);
  } else {
    start();
  }

  // Shiny repopulates fields when a plan is loaded from a file, which does not
  // fire an input event.
  if (window.$) {
    $(document).on("shiny:value shiny:updateinput", function () {
      setTimeout(function () { fitAll(document); }, 0);
    });
  }
})();

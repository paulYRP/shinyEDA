(function () {
  function closestElement(target, selector) {
    if (!target || !target.closest) {
      return null;
    }
    return target.closest(selector);
  }

  function setActiveSection(key, inputId) {
    var selectedKey = key || "home";

    document.querySelectorAll(".section-panel").forEach(function (panel) {
      panel.classList.toggle(
        "section-panel-active",
        panel.getAttribute("data-section-key") === selectedKey
      );
    });

    document.querySelectorAll(".nav-button").forEach(function (button) {
      button.classList.toggle(
        "active",
        button.getAttribute("data-nav-key") === selectedKey
      );
    });

    if (window.Shiny && inputId) {
      window.Shiny.setInputValue(inputId, selectedKey, { priority: "event" });
    }
  }

  function filterSidebar(searchInput) {
    var query = (searchInput.value || "").toLowerCase().trim();

    document.querySelectorAll(".nav-tree details").forEach(function (section) {
      var visibleButtons = 0;
      section.querySelectorAll(".nav-button").forEach(function (button) {
        var label = (button.getAttribute("data-nav-label") || button.textContent || "").toLowerCase();
        var visible = query.length === 0 || label.indexOf(query) !== -1;
        button.hidden = !visible;
        if (visible) {
          visibleButtons += 1;
        }
      });
      section.hidden = visibleButtons === 0;
    });
  }

  function getOptionLabel(input) {
    var label = input.closest("label");
    if (!label) {
      return input.value || "";
    }
    return label.textContent.replace(/\s+/g, " ").trim();
  }

  function updateCheckboxDropdown(dropdown) {
    if (!dropdown) {
      return;
    }

    var placeholder = dropdown.getAttribute("data-placeholder") || "Select values";
    var summary = dropdown.querySelector(".checkbox-dropdown-summary");
    var checked = Array.prototype.slice.call(
      dropdown.querySelectorAll("input[type='checkbox']:checked")
    );

    if (!summary) {
      return;
    }

    if (checked.length === 0) {
      summary.textContent = placeholder;
      dropdown.classList.remove("has-selection");
      return;
    }

    dropdown.classList.add("has-selection");
    if (checked.length <= 3) {
      summary.textContent = checked.map(getOptionLabel).join(", ");
      return;
    }

    summary.textContent = checked.length + " selected";
  }

  function clearCheckboxDropdownPosition(dropdown) {
    if (!dropdown) {
      return;
    }

    var menu = dropdown.querySelector(".checkbox-dropdown-menu");
    if (!menu) {
      return;
    }

    menu.style.left = "";
    menu.style.top = "";
    menu.style.width = "";
    menu.style.maxHeight = "";
  }

  function positionCheckboxDropdown(dropdown) {
    if (!dropdown || !dropdown.classList.contains("is-open")) {
      return;
    }

    var toggle = dropdown.querySelector(".checkbox-dropdown-toggle");
    var menu = dropdown.querySelector(".checkbox-dropdown-menu");
    if (!toggle || !menu) {
      return;
    }

    var rect = toggle.getBoundingClientRect();
    var gap = 4;
    var viewportMargin = 10;
    var availableBelow = window.innerHeight - rect.bottom - viewportMargin;
    var availableAbove = rect.top - viewportMargin;
    var openAbove = availableBelow < 180 && availableAbove > availableBelow;
    var maxHeight = Math.max(160, Math.min(320, openAbove ? availableAbove - gap : availableBelow - gap));
    var menuWidth = Math.max(rect.width, Math.min(520, menu.scrollWidth || rect.width));
    var left = Math.min(Math.max(viewportMargin, rect.left), window.innerWidth - menuWidth - viewportMargin);
    var top = openAbove ? rect.top - maxHeight - gap : rect.bottom + gap;

    menu.style.left = left + "px";
    menu.style.top = Math.max(viewportMargin, top) + "px";
    menu.style.width = menuWidth + "px";
    menu.style.maxHeight = maxHeight + "px";
  }

  function positionOpenCheckboxDropdowns() {
    document.querySelectorAll(".checkbox-dropdown.is-open").forEach(positionCheckboxDropdown);
  }

  function initializeCheckboxDropdowns(root) {
    var context = root || document;
    var dropdowns = Array.prototype.slice.call(context.querySelectorAll(".checkbox-dropdown"));
    if (context.matches && context.matches(".checkbox-dropdown")) {
      dropdowns.unshift(context);
    }

    dropdowns.forEach(function (dropdown) {
      if (!dropdown.getAttribute("data-checkbox-ready")) {
        dropdown.setAttribute("data-checkbox-ready", "true");
      }
      updateCheckboxDropdown(dropdown);
      positionCheckboxDropdown(dropdown);
    });
  }

  var initPending = false;

  function scheduleCheckboxDropdownInit() {
    if (initPending) {
      return;
    }

    initPending = true;
    var runInit = function () {
      initPending = false;
      initializeCheckboxDropdowns(document);
    };

    if (window.requestAnimationFrame) {
      window.requestAnimationFrame(runInit);
    } else {
      window.setTimeout(runInit, 0);
    }
  }

  function closeOtherDropdowns(activeDropdown) {
    document.querySelectorAll(".checkbox-dropdown.is-open").forEach(function (dropdown) {
      if (dropdown !== activeDropdown) {
        dropdown.classList.remove("is-open");
        clearCheckboxDropdownPosition(dropdown);
      }
    });
  }

  document.addEventListener("click", function (event) {
    var navButton = closestElement(event.target, ".nav-button");
    if (navButton) {
      event.preventDefault();
      setActiveSection(
        navButton.getAttribute("data-nav-key"),
        navButton.getAttribute("data-nav-input")
      );
      return;
    }

    var button = closestElement(event.target, ".checkbox-dropdown-toggle");
    var dropdown = closestElement(event.target, ".checkbox-dropdown");

    if (button && dropdown) {
      event.preventDefault();
      closeOtherDropdowns(dropdown);
      dropdown.classList.toggle("is-open");
      updateCheckboxDropdown(dropdown);
      if (dropdown.classList.contains("is-open")) {
        positionCheckboxDropdown(dropdown);
      } else {
        clearCheckboxDropdownPosition(dropdown);
      }
      return;
    }

    if (!dropdown) {
      closeOtherDropdowns(null);
    }
  });

  document.addEventListener("change", function (event) {
    var dropdown = closestElement(event.target, ".checkbox-dropdown");
    if (dropdown) {
      updateCheckboxDropdown(dropdown);
      positionCheckboxDropdown(dropdown);
    }
  });

  document.addEventListener("input", function (event) {
    var searchWrap = closestElement(event.target, ".sidebar-search");
    if (searchWrap) {
      filterSidebar(event.target);
    }
  });

  document.addEventListener("shiny:value", function () {
    scheduleCheckboxDropdownInit();
  });

  window.addEventListener("resize", positionOpenCheckboxDropdowns);
  window.addEventListener("scroll", positionOpenCheckboxDropdowns, true);

  var bodyObserver = new MutationObserver(function (mutations) {
    for (var i = 0; i < mutations.length; i += 1) {
      if (mutations[i].addedNodes.length > 0) {
        scheduleCheckboxDropdownInit();
        break;
      }
    }
  });

  document.addEventListener("DOMContentLoaded", function () {
    initializeCheckboxDropdowns(document);
    if (document.body) {
      bodyObserver.observe(document.body, { childList: true, subtree: true });
    }

    var activeButton = document.querySelector(".nav-button.active") || document.querySelector(".nav-button");
    if (activeButton) {
      setActiveSection(
        activeButton.getAttribute("data-nav-key"),
        activeButton.getAttribute("data-nav-input")
      );
    }
  });
})();

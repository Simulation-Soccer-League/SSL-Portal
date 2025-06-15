box::use(
  shiny[div, icon, numericInput, span, tag, tagList],
)


#' @export
flexCol <- function(cont, style = "", onclick = "") {
  tag("div", varArgs = list(
    cont,
    class = "flex-col-wrapper",
    style = style,
    onclick = onclick
  ))
}

#' @export
flexRow <- function(cont, style = "", onclick = "") {
  tag("div", varArgs = list(
    cont,
    class = "flex-row-wrapper",
    style = style,
    onclick = onclick
  ))
}

closeNarrowMenuJS <- "
  var mobileNav = document.querySelector('.nav-container-narrow');
  var mobileNavToggle = document.querySelector('.nav-toggle');

  mobileNav.style.maxWidth = '0px';
  mobileNavToggle.style.left = '0px';
  mobileNavToggle.querySelector('.nav-toggle-icon_closed').style.display = 'block';
  mobileNavToggle.querySelector('.nav-toggle-icon-open').style.display = 'none';
"

#' @export
navMenu <- function(cont, label = "", items = list(), showItems = FALSE) {
  if (!missing(cont) && label == "") {
    tag("div", varArgs = list(cont, class = "nav-menu", onclick = closeNarrowMenuJS))
  } else if (length(label) > 0) {
    itemsClassNames <- c("nav-menu-items", if (showItems) " show-items" else "")

    div(
      class = "nav-menu",
      role = "button",
      onclick = c("
        const allMenuItems = document.querySelectorAll('.nav-menu-items');
        const allSubMenuItems = document.querySelectorAll('.nav-menu-sub-items');
        const childMenuItems = this.querySelector('.nav-menu-items');
        const childSubMenuItems = this.querySelector('.nav-menu-sub-items');

        if (childMenuItems) {
          const isClosed = getComputedStyle(childMenuItems).height === '0px';

          // Close all other open menus
          if (isClosed) {
            [...allMenuItems, ...allSubMenuItems].forEach(item => {
              if (item !== childMenuItems && item !== childSubMenuItems) {
                item.style.height = '0px';
              }
            });
          }

          childMenuItems.style.height = 'max-content';
        } else {
          ", closeNarrowMenuJS, "
        }
      "),
      tagList(
        flexRow(
          style = "align-items: center; gap: 4px;",
          tagList(
            span(label),
            icon("caret-down")
          )
        ),
        if (length(items) > 0) {
          div(
            class = itemsClassNames,
            role = "button",
            flexCol(
              tagList(
                lapply(items, function(item) {
                  div(
                    class = "nav-menu-item",
                    onclick = c("
                      const allSubMenuItems = document.querySelectorAll('.nav-menu-sub-items');
                      const childSubMenuItems = this.querySelector('.nav-menu-sub-items');

                      if (childSubMenuItems) {
                        const isClosed = getComputedStyle(childSubMenuItems).height === '0px';

                        // Close all other open menus
                        if (isClosed) {
                          allSubMenuItems.forEach(item => {
                            if (item !== childSubMenuItems) {
                              item.style.height = '0px';
                            }
                          });
                        }

                        childSubMenuItems.style.height = 'max-content';
                      } else {
                      ", closeNarrowMenuJS, "
                      }
                    "),
                    item
                  )
                })
              )
            )
          )
        }
      )
    )
  }
}

#' @export
navMenuItem <- function(cont, label = "", subItems = list()) {
  if (length(subItems) > 0) {
    tagList(
      flexRow(
        style = "align-items: center; justify-content: space-between; gap: 4px;",
        tagList(
          span(label, role = "button"),
          div(
            class = "nav-menu-item-caret-right",
            icon("caret-right")
          ),
          div(
            class = "nav-menu-item-caret-down",
            icon("caret-down")
          )
        )
      ),
      div(
        class = "nav-menu-sub-items",
        role = "button",
        div(
          tagList(
            lapply(subItems, function(item) {
              div(
                class = "nav-menu-sub-item",
                onclick = closeNarrowMenuJS,
                item
              )
            })
          )
        )
      )
    )
  } else {
    cont
  }
}

#' @export
numericStepper <- function(inputId, value, min = 5, max = 20, step = 1, onChange = "", disabled = FALSE) {
  tag("div", varArgs = list(
    class = "numeric-stepper",
    div(
      class = "numeric-stepper-button minus-button",
      style = if (disabled) "visibility: hidden;" else "",
      role = "button",
      onclick = paste0("
        input = document.querySelector('input[id=", inputId, "]');
        currentValue = parseInt(input.value, 10);
        newValue = Math.max(", min, ", currentValue - ", step, ");",
        "this.nextElementSibling.textContent = newValue;",
        "input.value = newValue; input.dispatchEvent(new Event('change'));",
        onChange
      ),
      icon("minus")
    ),
    div(
      class = "numeric-stepper-value",
      value
    ),
    div(
      class = "numeric-stepper-button plus-button",
      style = if (disabled) "visibility: hidden;" else "",
      role = "button",
      onclick = paste0("
        input = document.querySelector('input[id=", inputId, "]');",
        "currentValue = parseInt(input.value, 10);
        newValue = Math.min(", max, ", currentValue + ", step, ");",
        "this.previousElementSibling.textContent = newValue;",
        "input.value = newValue; input.dispatchEvent(new Event('change'));",
        onChange
      ),
      icon("plus")
    ),
    # Hidden input to bind the value to an input ID
    div(
      style = "display: none;",
      numericInput(
        inputId,
        label = NULL,
        value = value,
      )
    )
  )
)}

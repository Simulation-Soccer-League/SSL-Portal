box::use(
  shiny[div, icon, span, tagList],
)


#' @export
flexCol <- function(cont, style = "", onclick = "") {
  shiny::tag("div", varArgs = list(
    cont,
    class = "flex-col-wrapper",
    style = style,
    onclick = onclick
  )
)}

#' @export
flexRow <- function(cont, style = "", onclick = "") {
  shiny::tag("div", varArgs = list(
    cont,
    class = "flex-row-wrapper",
    style = style,
    onclick = onclick
  )
)}

closeNarrowMenuJS <- "
  var mobileNav = document.querySelector('.nav-container_narrow');
  var mobileNavToggle = document.querySelector('.nav-toggle');

  mobileNav.style.maxWidth = '0px';
  mobileNavToggle.style.left = '0px';
  mobileNavToggle.querySelector('.nav-toggle-icon_closed').style.display = 'block';
  mobileNavToggle.querySelector('.nav-toggle-icon_open').style.display = 'none';
"

#' @export
navMenu <- function(cont, label = "", items = list(), showItems = FALSE) {
  if (!missing(cont) && label == "") {
    shiny::tag("div", varArgs = list(cont, class = "nav-menu", onclick = closeNarrowMenuJS))
  } else if (length(label) > 0) {
    itemsClassNames <- c("nav-menu_items", if (showItems) " show-items" else "")

    div(
      class = "nav-menu",
      role = "button",
      onclick = c("
        const allMenuItems = document.querySelectorAll('.nav-menu_items');
        const allSubMenuItems = document.querySelectorAll('.nav-menu_sub-items');
        const childMenuItems = this.querySelector('.nav-menu_items');
        const childSubMenuItems = this.querySelector('.nav-menu_sub-items');

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
                tags_list <- lapply(items, function(item) {
                  div(
                    class = "nav-menu_item",
                    onclick = c("
                      const allSubMenuItems = document.querySelectorAll('.nav-menu_sub-items');
                      const childSubMenuItems = this.querySelector('.nav-menu_sub-items');

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
            class = "nav-menu_item-caret-right",
            icon("caret-right")
          ),
          div(
            class = "nav-menu_item-caret-down",
            icon("caret-down")
          )
        )
      ),
      div(
        class = "nav-menu_sub-items",
        role = "button",
        div(
          tagList(
            tags_list <- lapply(subItems, function(item) {
              div(
                class = "nav-menu_sub-item",
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

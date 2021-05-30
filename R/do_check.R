
# Note that `use_y` could be FALSE for a true test set, without any actual observed `y`.
# This package is a bit opinionated by required there to be an `id`.
.do_check <-
  function(data = NULL,
           x = NULL,
           y = NULL,
           col_y = NULL,
           col_id = NULL,
           id = NULL,
           col_wt = NULL,
           wt = NULL,
           drop = TRUE,
           objective = NULL,
           use_y = FALSE,
           is_prediction = !use_y) {

    # browser()
    has_data <- !is.null(data)
    has_col_id <- !is.null(col_id)
    has_id <- !is.null(id)
    has_x <- !is.null(x)
    has_col_y <- !is.null(col_y)
    has_y <- !is.null(y)
    has_col_wt <- !is.null(col_wt)
    has_wt <- !is.null(wt)

    if(!has_y & !has_col_y & use_y) {
      use_y <- FALSE
      .display_warning(
        'Cannot `use_y` if both `y` and `col_y` are NULL. Setting `use_y = FALSE`'
      )
    }

    if((has_col_y | has_y) & !use_y) {
      use_y <- FALSE
      .display_warning(
        'Either or both of `col_y` and `y` are not NULL. Will not use either since `use_y = FALSE`.'
      )
      col_y <- NULL
      has_col_y <- FALSE
      y <- NULL
      has_y <- FALSE
    }

    if(has_data) {

      nms <- data %>% names()

      # if(use_y | has_col_y | has_y) {
      if(use_y) {
        assertthat::assert_that(has_col_y | has_y, msg = glue::glue('Must provide `col_y` or `y` if `use_y = TRUE`.'))
        if(has_col_y & !has_y) {

          assertthat::assert_that(has_col_y, msg = '`col_y` cannot be `NULL`.')
          assertthat::assert_that(is.character(col_y), msg = glue::glue('`col_y` must be a character, not a {class(col_y)}.'))
          assertthat::assert_that(length(col_y) == 1L, msg = '`col_y` must have length 1, not {length(col_y)}.')
          assertthat::assert_that(any(col_y == nms), msg = glue::glue('`col_y = {col_y} is not in `names(data)` ({nms}).'))
          col_y_sym <- sym(col_y)
          y <- data[[col_y]]
        } else {
          y_is_df <- any(class(y) == 'data.frame')

          if(y_is_df) {
            n_col_y <- ncol(y)
            assertthat::assert_that(n_col_y == 1L | has_col_y, msg = glue::glue('If `y` is passed in as a data.frame, it should have only one column (not, {n_col_y}, unless `col_y` is also specified.'))
            if(n_col_y == 1L) {
              y <- y %>% dplyr::pull(1)
            } else {
              assertthat::assert_that(is.character(col_y), msg = glue::glue('`col_y` must be a character, not a {class(col_y)}.'))
              assertthat::assert_that(length(col_y) == 1L, msg = '`col_y` must have length 1, not {length(col_y)}.')
              assertthat::assert_that(any(col_y == nms), msg = glue::glue('`col_y = {col_y} is not in `names(data)`.'))
              y <- y[[col_y]]
              data <- dplyr::bind_cols(data, tibble::tibble(y) %>% rlang::set_names(col_y))
            }

          } else {
            y <- y %>% as.vector()
          }
        }

        len_y <- length(y)
        n_row <- nrow(data)
        assertthat::assert_that(len_y == n_row, msg = glue::glue('`y` should have the same length ({len_y}) as the number of rows in `data` ({n_row}).'))

        if(drop & !has_col_y) {
          n_before <- nrow(data)
          data <- data %>% dplyr::filter(!is.na(!!col_y_sym))
          n_after <- nrow(data)
          y <- y[!is.na(y)]
          if(n_before != n_after) {
            .display_warning('# of rows before dropping: {n_before}
                           # of rows after dropping: {n_after}')
          }
        }
      }

      assertthat::assert_that(has_col_id | has_id, msg = glue::glue('Must provide `col_id` or `id`.'))
      if(has_col_id & !has_id) {
        assertthat::assert_that(has_col_id, msg = '`col_id` cannot be `NULL`.')
        assertthat::assert_that(is.character(col_id), msg = glue::glue('`col_id` must be a character, not a {class(col_id)}.'))
        assertthat::assert_that(length(col_id) == 1L, msg = '`col_id` must have length 1, not {length(col_id)}.')
        assertthat::assert_that(any(col_id == nms), msg = glue::glue('`col_id = {col_id} is not in `names(data)`.'))
        col_id_sym <- col_id %>% sym()
        id <- data[[col_id]]
      } else {
        id_is_df <- any(class(id) == 'data.frame')

        if(id_is_df) {
          n_col_id <- ncol(id)
          assertthat::assert_that(n_col_id == 1L | has_col_id, msg = glue::glue('If `id` is passed in as a data.frame, it should have onlid one column (not, {n_col_id}, unless `col_id` is also specified.'))
          if(n_col_id == 1L) {
            id <- id %>% dply::pull(1)
          } else {
            assertthat::assert_that(is.character(col_id), msg = glue::glue('`col_id` must be a character, not a {class(col_id)}.'))
            assertthat::assert_that(length(col_id) == 1L, msg = '`col_id` must have length 1, not {length(col_id)}.')
            assertthat::assert_that(any(col_id == nms), msg = glue::glue('`col_id = {col_id} is not in `names(data)`.'))
            id <- id[[col_id]]
          }
          data <- dplyr::bind_cols(data, tibble::tibble(id) %>% rlang::set_names(col_id))
        } else {
          id <- id %>% as.vector()
        }
      }

      len_id <- length(id)
      # Re-generate this variable in case `use_y = FALSE` of if `drop = TRUE` and there are actually rows dropped.
      n_row <- nrow(data)
      assertthat::assert_that(len_id == n_row, msg = glue::glue('`id` should have the same length ({len_id}) as the number of rows in `data` ({n_row}).'))

      if(has_col_wt | has_wt) {
        if(has_col_wt & !has_wt) {
          assertthat::assert_that(has_col_wt, msg = '`col_wt` cannot be `NULL`.')
          assertthat::assert_that(is.character(col_wt), msg = glue::glue('`col_wt` must be a character, not a {class(col_wt)}.'))
          assertthat::assert_that(length(col_wt) == 1L, msg = '`col_wt` must have length 1, not {length(col_wt)}.')
          assertthat::assert_that(any(col_wt == nms), msg = glue::glue('`col_wt = {col_wt} is not in `names(data)`.'))
          col_wt_sym <- col_wt %>% sym()
          wt <- data[[col_wt]]
        } else {
          wt_is_df <- ant(class(wt) == 'data.frame')

          if(wt_is_df) {
            n_col_wt <- ncol(wt)
            assertthat::assert_that(n_col_wt == 1L | has_col_wt, msg = glue::glue('If `wt` is passed in as a data.frame, it should have only one column (not, {n_col_wt}, unless `col_wt` is also specified.'))
            if(n_col_wt == 1L) {
              wt <- wt %>% dply::pull(1)
            } else {
              assertthat::assert_that(is.character(col_wt), msg = glue::glue('`col_wt` must be a character, not a {class(col_wt)}.'))
              assertthat::assert_that(length(col_wt) == 1L, msg = '`col_wt` must have length 1, not {length(col_wt)}.')
              assertthat::assert_that(any(col_wt == nms), msg = glue::glue('`col_wt = {col_wt} is not in `names(data)`.'))
              wt <- wt[[col_wt]]
              data <- dplyr::bind_cols(data, tibble::tibble(wt) %>% rlang::set_names(col_wt))
            }

          } else {
            wt <- wt %>% as.vector()
          }
        }
        len_wt <- length(wt)
        assertthat::assert_that(len_wt == n_row, msg = glue::glue('`wt` should have the same length ({len_wt}) as the number of rows in `data` ({n_row}).'))
      }

      x <- data %>% dplyr::select(-dplyr::any_of(c(col_id, col_y, col_wt)))

    } else {

      assertthat::assert_that(has_x, msg = '`x` cannot be NULL if `data` is NULL.')

      if(use_y | has_col_y | has_y) {
        # assertthat::assert_that(has_col_y | has_y, msg = glue::glue('Must provide `col_y` or `y` if `use_y = TRUE`.'))
        assertthat::assert_that(has_col_y | has_y, msg = '`col_y` or `y` cannot both be NULL.')
        assertthat::assert_that(has_y, msg = '`col_y` is specified but `y` is NULL.')
        y_is_df <- any(class(y) == 'data.frame')

        if(y_is_df) {
          n_col_y <- ncol(y)
          assertthat::assert_that(n_col_y == 1L | has_col_y, msg = glue::glue('If `y` is passed in as a data.frame, it should have only one column (not, {n_col_y}, unless `col_y` is also specified.'))
          if(n_col_y == 1L) {
            y <- y %>% dplyr::pull(1)
          } else {
            assertthat::assert_that(is.character(col_y), msg = glue::glue('`col_y` must be a character, not a {class(col_y)}.'))
            assertthat::assert_that(length(col_y) == 1L, msg = '`col_y` must have length 1, not {length(col_y)}.')
            assertthat::assert_that(any(col_y == nms), msg = glue::glue('`col_y = {col_y} is not in `names(data)`.'))
            y <- y[[col_y]]
          }

        } else {
          y <- y %>% as.vector()
        }

        len_y <- length(y)
        n_row <- nrow(x)
        assertthat::assert_that(len_y == n_row, msg = glue::glue('`y` should have the same length ({len_y}) as the number of rows in `x` ({n_row}).'))
      }

      assertthat::assert_that(has_col_id | has_id, msg = '`col_id` or `id` cannot both be NULL.')
      assertthat::assert_that(has_id, msg = '`col_id` is specified but `id` is NULL.')
      id_is_df <- any(class(id) == 'data.frame')

      if(id_is_df) {
        n_col_id <- ncol(id)
        assertthat::assert_that(n_col_id == 1L | has_col_id, msg = glue::glue('If `id` is passed in as a data.frame, it should have onlid one column (not, {n_col_id}, unless `col_id` is also specified.'))
        if(n_col_id == 1L) {
          id <- id %>% dplyr::pull(1)
        } else {
          assertthat::assert_that(is.character(col_id), msg = glue::glue('`col_id` must be a character, not a {class(col_id)}.'))
          assertthat::assert_that(length(col_id) == 1L, msg = '`col_id` must have length 1, not {length(col_id)}.')
          assertthat::assert_that(any(col_id == nms), msg = glue::glue('`col_id = {col_id} is not in `names(data)`.'))
          id <- id[[col_id]]
        }

      } else {
        id <- id %>% as.vector()
      }

      len_id <- length(id)
      # Re-generate this variable in case `use_y = FALSE`.
      n_row <- nrow(x)
      assertthat::assert_that(len_id == n_row, msg = glue::glue('`id` should have the same length ({len_id}) as the number of rows in `x` ({n_row}).'))

      if(has_col_wt | has_wt) {

        assertthat::assert_that(has_wt, msg = '`col_wt` is specified but `data` is NULL and `wt` is NULL.')

        wt_is_df <- any(class(wt) == 'data.frame')
        if(wt_is_df) {
          n_col_wt <- ncol(wt)
          assertthat::assert_that(n_col_wt == 1L | has_col_wt, msg = glue::glue('If `wt` is passed in as a data.frame, it should have only one column (not, {n_col_wt}, unless `col_wt` is also specified.'))
          if(n_col_wt == 1L) {
            wt <- wt %>% dplyr::pull(1)
          } else {
            assertthat::assert_that(is.character(col_wt), msg = glue::glue('`col_wt` must be a character, not a {class(col_wt)}.'))
            assertthat::assert_that(length(col_wt) == 1L, msg = '`col_wt` must have length 1, not {length(col_wt)}.')
            assertthat::assert_that(any(col_wt == nms), msg = glue::glue('`col_wt = {col_wt} is not in `names(data)`.'))
            wt <- wt[[col_wt]]
          }
        } else {
          wt <- wt %>% as.vector()
        }
        len_wt <- length(wt)
        assertthat::assert_that(len_wt == n_row, msg = glue::glue('`wt` should have the same length ({len_wt}) as the number of rows in `x` ({n_row}).'))
      }
      data <- x
    }

    if(!has_col_wt & !has_wt) {
      n_row <- nrow(x)
      wt <- rep(1, n_row)
    }

    check_y <- (has_col_y | has_y)
    has_objective <- !is.null(objective)
    if(has_objective) {
      type_objective <- if(str_detect(objective, '^binary[:]')) {
        'binary'
      } else if (str_detect(objective, '^multi[:]')) {
        .display_warning(
          'Make sure you\'ve defined `num_class` (since this looks like a multinomial problem).'
        )
        'multiclass'
      } else {
        'continuous'
      }
    }

    if(check_y) {
      y_unique <- unique(y)
      n_y <- length(y_unique)
      y_is_numeric <- is.numeric(y)

      type_y <- if(n_y == 2L) {
        'binary'
      } else if (n_y < (length(y) * 0.5)) {
        .display_warning(
          'Make sure you\'ve defined `num_class` (since this looks like a multinomial problem).'
        )
        'multiclass'
      } else {
        'continuous'
      }

      if(has_objective) {
        assertthat::assert_that(type_y == type_objective, msg = glue::glue('Type inferred from `y` ({type_y}) is not the same as the type inferred from `objective` ({type_objective}).'))
      }

      if(type_y != 'continuous') {
        has_0 <- any(y_unique == 0)
        assertthat::assert_that(has_0, msg = '`y` should include a 0 class.')
      }

      # objective_makes_sense <- if(type_y == 'binary') {
      #   objective %>% str_detect('^binary[:]')
      # } else if (type_y == 'multiclass') {
      #   objective %>% str_detect('^multi[:]')
      # } else if (type_y == 'continuous') {
      #   objective %>% str_detect('^reg[:]')
      # }
      #
      # assertthat::assert_that(objective_makes_sense, msg = glue::glue('It doesn\'t seem like your `objective` ({objective}) makes sense given the type of `y` ({type_y})))'))
    } else if(has_objective) {
      type_y <- 'unknown'
    } else {
      .display_error(
        'You should provide some hint of the class of `y` by either
        (1) providing `y` (or `col_y` and `data`) and setting `use_y`,
        (2) explicitly setting `objective`, or
        (3) passing in a fit with non-NULL `fit$params$objective$`.'
      )
      type_y <- 'unknown'
    }

    list(
      data = data,
      x = x,
      y = y,
      # TODO: Return this? Need to add logic where objective is passed in and used as a backup.
      type_y = type_y,
      col_y = col_y %||% '.y',
      id = id,
      col_id = col_id %||% '.id',
      wt = wt,
      col_wt = col_wt %||% '.wt',
      x_mat = x %>% .df2mat()
    )
  }

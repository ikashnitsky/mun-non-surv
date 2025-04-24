#===============================================================================
# 2025-04-24 -- mun-non-surv
# Own functions
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# kannisto smooth ---------------------------------------------------------

# Kannisto optimization
kannisto_loglk <- function(theta, D, E, x, pl = 1){
    a <- theta[1]
    b <- theta[2]
    mu <- (a*exp(b*(x-80)))/(1+(a/pl)*exp(b*(x-80)))
    loglk <- (D*log(mu)-(E*mu))+1
    return(-sum(loglk))
}

# a function that performs the smooth
kannisto_smooth <- function(mx, Dx, Ex, plateau = 1) {

    oldage = 80:110

    optim <-
        optim(
            par = c(.044, .044),
            fn = kannisto_loglk,
            method = "L-BFGS-B",
            lower = 0,
            upper = 5,
            x = oldage,
            D = Dx,
            E = Ex,
            pl = plateau
        )

    mx_smooth <-
        ((optim$par[1]*exp(optim$par[2]*(oldage-80)))/
             (1+(optim$par[1]/plateau)*exp(optim$par[2]*(oldage-80))))

    return(c(mx[1:15], mx_smooth[16:31]))
}


# life table --------------------------------------------------------------

# a(0) from HMD protocol
a0sex <- function(m0,sex){ #sex: m=male f=female
    if (sex=="m"){
        if(m0<0.02300){
            a0 <- 0.14929-1.99545*m0 }
        else if (m0>=0.0230 & m0<0.08307){
            a0 <- 0.02832+3.26021*m0}
        else if (m0>=0.08307){
            a0 <- 0.29915}
    }
    else {
        if(m0<0.01724){
            a0 <- 0.14903-2.05527*m0 }
        else if (m0>=0.01724 & m0<0.06891){
            a0 <- 0.04667+3.88089*m0}
        else if (m0>=0.06891) {
            a0 <- 0.31411}
    }
    return(a0)
}

# life table for single age groups
lt <- function(mx, sex = c("f", "m"), a0 = NULL) {
    x <-  seq_along(mx) - 1
    ax <- rep(.5, length(x))
    if(!is.null(a0)){ax[1] <- a0}
    else {ax[1] <- a0sex(mx[1], sex)} # own func defined above
    nx = c(diff(x),NA)
    qx = ifelse(nx*mx/(1+(nx-ax)*mx)>1, 1, nx*mx/(1+(nx-ax)*mx))
    qx[length(qx)] = 1
    px = 1-qx
    lx = head(cumprod(c(1, px)), -1)
    dx = c(-diff(lx), tail(lx, 1))
    Lx = ifelse(mx==0, lx*nx, dx/mx)
    Tx = rev(cumsum(rev(Lx)))
    ex = Tx/lx
    ax.update <- c(ax[-length(ax)],ex[length(ex)])
    return(
        tibble(
            age = x,
            mx = mx,
            qx = qx,
            ax = ax.update,
            lx = lx,
            dx = dx,
            lx_2 = Lx,
            tx = Tx,
            ex = ex
        )
    )
}

# function for only e0
e0 <- function(mx) {
    lifet <- lt(mx, a0 = .3)
    return(lifet$ex[1])
}
# for a0=.3 see: https://twitter.com/ikashnitsky/status/1370531536320266243


#
# # decompose changes in e0 gap ---------------------------------------------
#
# decompose_gap <- function (mx1, mx2, mx3, n, periods){
#     #create output data frame
#     output <- data.frame(age=n,
#                          age.groups=age_grouping(n),
#                          changegap1=NA,
#                          contribution1=NA,
#                          changegap2=NA,
#                          contribution2=NA,
#                          years=toString(periods))
#
#     #decompose the gap between time 1 and time 2
#     decomposegap1 <- stepwise_replacement(func = e0gap,
#                                           pars1 = mx1, pars2 = mx2,
#                                           symmetrical = TRUE, direction = "up")
#     #decompose the gap between time 2 and time 3
#     decomposegap2 <- stepwise_replacement(func = e0gap,
#                                           pars1 = mx2, pars2 = mx3,
#                                           symmetrical = TRUE, direction = "up")
#
#     #add up the age contributions 1+112, 2+113, 3+114...
#     for (i in 1:length(n)) {
#         output$contribution1[i] <- decomposegap1[i]+decomposegap1[i+length(n)]
#         output$contribution2[i] <- decomposegap2[i]+decomposegap2[i+length(n)]
#     }
#     output$changegap1 <- sum(decomposegap1)
#     output$changegap2 <- sum(decomposegap2)
#     return(output)
# }


# TOPALS ------------------------------------------------------------------

# https://github.com/schmert/TOPALS/blob/master/TOPALS_fitting_with_grouped_data.pdf


TOPALS_fit <-  function( N, D, std,
                       age_group_bounds   = 0:100,
                       knot_positions     = c(0,1,10,20,40,70),
                       penalty_precision  = 2,
                       max_iter           = 50,
                       alpha_tol          = .00005,
                       details            = FALSE) {

    require(splines)

    ## single years of age from 0 to (A-1)
    A   = length(std)
    age = 0:(A-1)

    ## B is an AxK matrix. Each column is a linear B-spline basis function
    B      = bs( age, knots=knot_positions, degree=1 )
    K = ncol(B)

    D1 = diff( diag(K), diff=1)
    P  = penalty_precision * crossprod(D1)

    ## number and width of age groups
    G     = length(age_group_bounds)-1
    nages = diff(age_group_bounds)

    ## weighting matrix for mortality rates (assumes uniform
    ## distribution of single-year ages within groups)
    W = matrix(0, nrow=G, ncol=A,
               dimnames=list(head(age_group_bounds,-1) , age))

    offset = 0
    for (g in 1:G) {
        W[g, offset + 1:nages[g]] = 1/nages[g]
        offset = offset + nages[g]
    }

    ## penalized log lik function
    Q = function(alpha) {
        M = W %*% exp( std + B %*% alpha)
        likelihood = sum(D * log(M) - N * M)
        penalty    = 1/2 * t(alpha) %*% P %*% alpha
        return( likelihood - penalty )
    }

    #------------------------------------------------
    # iteration function:
    # next alpha vector as a function of current alpha
    #------------------------------------------------
    next_alpha = function(alpha) {
        mu = as.vector( exp( std + B %*% alpha))
        M  = as.vector( W %*% mu)

        Dhat = N * M

        X = W %*% diag(mu) %*% B
        A = diag(N/M)

        y = (D-Dhat)/N + X %*% alpha

        updated_alpha = solve( t(X) %*% A %*% X + P, t(X) %*% A %*% y)
        return(as.vector(updated_alpha))
    }

    ## main iteration:
    a = rep(0, K)

    niter = 0
    repeat {
        niter      = niter + 1
        last_param = a
        a          = next_alpha( a )  # update
        change     = a - last_param

        converge = all( abs(change) < alpha_tol)
        overrun  = (niter == max_iter)

        if (converge | overrun) { break }

    } # repeat

    if (details | !converge | overrun) {
        if (!converge) print('did not converge')
        if (overrun) print('exceeded maximum number of iterations')

        mu    = as.vector( exp(std + B %*% a))
        M     = as.vector( W %*% mu )
        dhat  = N * M

        X     = W %*% diag(mu) %*% B
        A     = diag(N/M)

        covar = solve( t(X) %*% A %*% X + P)

        return( list( alpha             = a,
                      D                 = D,
                      N                 = N,
                      age_group_bounds  = age_group_bounds,
                      knots             = knot_positions,
                      std               = std,
                      B                 = B,
                      logm              = std + B %*% a,
                      covar             = covar,
                      Qvalue            = Q(a),
                      converge          = converge,
                      maxiter           = overrun))
    } else return( a)

} # TOPALS_fit


# show TOPALS -------------------------------------------------------------



# a hardcoded function to show how TOPALS works
display_topals <- function(mun = "Odense", hue = "#33BABA") {

    this_subset <- dk_fit %>%
        filter(name == mun, sex == "b", period == "2015-19")

    this_subtitle <- paste(
        sum(this_subset$death),
        'deaths to',
        prettyNum(sum(this_subset$exposure), big.mark = ",", scientific = FALSE),
        'person-years'
    )


    out <- this_subset %>%
        ggplot(aes(x = age)) +
        geom_line(aes(y = logmx_std), size = 3/4)+
        geom_point(
            aes(y = log(death/exposure)),
            color = hue,
            shape = 18
        )+
        geom_line(
            aes(y = logmx_fit),
            color = hue,
            size = 2, alpha = .5
        ) +
        coord_cartesian(
            xlim = c(-1, 100), ylim = c(-12, 0), expand = F
        )+
        scale_x_continuous(breaks = seq(0, 100, 25)) +
        scale_y_continuous(breaks = seq(-12, 0, 2)) +
        labs(
            subtitle = this_subtitle,
            x = "Age", y = "Log Death Rate"
        )+
        theme(
            plot.subtitle = element_text(face = 2, size = 11)
        )+
        annotate("text", x = 5, y = -1, label = mun, size = 10,
                 fontface = 2, family = "ah", color = hue,
                 alpha = .5, hjust = 0)

    return(out)
}

# read local HMD ----------------------------------------------------------


# path to local HMD
hmdpath <- fs::as_fs_path("/data/hmd/")

# a function to read ONE file
fread_hmd <- function(x) x %>%
    data.table::fread(skip = 2, na.strings = ".") %>%
    mutate(Age = Age %>% str_remove("\\+") %>% as.integer()) %>%
    mutate_all(as.numeric)

# a function to read in the whole directory
fread_hmd_dir <- function(thedir) {
    require(fs)
    require(magrittr)
    require(janitor)

    suppressWarnings(
        thedir %>%
            dir_ls() %>%
            map_df(fread_hmd, .id = "country") %>%
            janitor::clean_names() %>%
            mutate(
                country = country %>%
                    # remove everything in the string before the last forward slash
                    str_remove("^.*/") |>
                    #  remove everything after the first dot
                    str_remove("\\..*$"),
                age = age %>% str_remove("\\+") %>% as.integer()
            )
    )
}


# inset map DK ------------------------------------------------------------

# a function to create inset zoom stamp for Copenhagen area

# wrap inset CPH map as a function
inset_cph_box <- function(map_gg, stamp_size = .37, x = .43, y = .50) {
    require(ggplot2)
    require(sf)
    require(cowplot)

    # the box
    chp_box <- st_buffer(st_point(c(715e3,6180e3)), dist = 25e3) %>%
        st_bbox() %>%
        st_as_sfc() %>%
        st_set_crs(3044)

    # add the box to the map
    map_box <- map_gg +
        geom_sf(data = chp_box, fill = NA, color = "grey25")

    # Zoom Copenhagen area
    map_zoom <- map_gg +
        coord_sf(xlim = c(690e3, 740e3), ylim = c(6155e3, 6205e3))+
        theme_void()+
        labs(title = NULL, subtitle = NULL, caption = NULL)+
        theme(
            legend.position = "none",
            panel.border = element_rect(color = "grey25", fill = NA, size = 1)
        )

    # compose map
    map_out <- ggdraw()+
        draw_plot(map_box)+
        draw_plot(map_zoom, x = x, y = y,
                  width = stamp_size, height = stamp_size)

    return(map_out)
}


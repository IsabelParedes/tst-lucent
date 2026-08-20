## These functions are emulating the ask/tell hack. If you put ask.Y function as argument instead of a true function (like sin), it will wait as long as you put the Y values using tell.Y() in the same directory. So a 2nd R session will be used to get the X values asked (using ask.X) and then call tell.Y() which will unlock first session. This finally allows an asynchronized IO between many R/matlab/... sessions.
#' @author Y. Richet, from an idea by D. Sinoquet. Async IO principle was defined by G. Pujol.
#' @test x=matrix(runif(10)); f=sin; print(f(x)); parallel::mcparallel({tell.Y(f(ask.X()))}); Sys.sleep(1); print(ask.Y(x));
#' @test x=matrix(runif(10),ncol=2); f=function(X)rowSums(sin(X)); print(f(x)); parallel::mcparallel({tell.Y(f(ask.X()))}); Sys.sleep(1); print(ask.Y(x));
#' @test x=matrix(runif(10),ncol=2); f=function(X)rowSums(sin(X)); print(f(x));library(future); plan("multisession",workers=2); future(lazy=FALSE,{tell.Y(f(ask.X()))}); Sys.sleep(1); print(ask.Y(x));
#' @examples
#' f=sin; 
#' future(evaluator=plan("multisession"),{
#'         while(TRUE) {
#'                 tell.Y(f(ask.X()))
#'         }
#' }); 
#' optim(par=.5, fn=ask.Y, lower=0, upper=pi, method="L-BFGS-B")
#' 
#' f=sin; 
#' parallel::mcparallel({
#'         while(TRUE) {
#'                 tell.Y(f(ask.X()))
#'         }
#' }); 
#' optim(par=.5, fn=ask.Y, lower=0, upper=pi, method="L-BFGS-B")
#' 
#' optim(par=c(.5,.5), fn=function(x) ask.Y(matrix(x,ncol=2)), lower=c(0,0), upper=c(pi,pi), method="L-BFGS-B")

.default_dev.path = "./"

.trace = FALSE

# number of seconds to wait between checks of the files (used by ask.Y, ask.X)
.sleep.step = 1

# timeout before process 'ask.Y' or 'ask.X' is stopped
.timeout = 400000 # about 1 week

#' Writes a given matrix to a 'X.todo' file, and returns matrix which is read in 'Y.done' file (process is stopped if file doesn't exist after a given timeout)
#' @param x the object to be written, preferably a matrix or data frame.
#' @param id suffix of file, should be a unique identifier
#' @param dev.X prefix of file to store (default is 'X.todo')
#' @param dev.Y prefix of file to read (default is 'Y.done')
#' @param timeout tells if 'file' must be removed (default: ~ 1 week)
#' @param clean tells if 'file' must be removed
#'
ask.Y <-
    function(x,
             id = 0,
             dev.X = "X.todo",
             dev.Y = "Y.done",
             dev.path = NULL,
             sleep.step = .sleep.step,
             sleep.init = 0.1,
             trace = ifelse(exists(".trace"), .trace, TRUE),
             timeout = .timeout,
             clean = T) {
        if (trace)
            cat("?Y(", paste0(collapse = ",", x), ") ")
        
        xFile = X.file(
            id = id,
            dev.X = dev.X,
            dev.path = dev.path
        )
        write.io(x, file = xFile)
        
        Sys.sleep(sleep.init)
        t = 0
        lock = paste0("ask.Y_", id)
        if (!is.null(dev.path)) {
            lock = file.path(dev.path, lock)
        }
        file.create(lock)
        yFile = Y.file(
            id = id,
            dev.Y = dev.Y,
            dev.path = dev.path
        )
        while (!file.exists(file = yFile) & (timeout > 0 & t < timeout)) {
            Sys.sleep(sleep.step)
            t = t + sleep.step
            if (!file.exists(lock)) {
                del.file(xFile)
                del.file(yFile)
                stop("ask.Y break !")
            }
            if (trace)
                cat(".")
        }
        del.file(lock)
        if (timeout > 0 & t >= timeout) {
            del.file(xFile)
            del.file(yFile)
            stop("ask.Y timeout !")
        }
#        Sys.sleep(sleep.step)
        if (trace)
            cat(",")
        
        y = read.io(file = yFile, clean = clean)
        
        if (trace)
            cat("(", paste0(collapse = ",", y), ")")
        
        return(y)
    }

ask.Y.unlock <-
    function(id = 0,
             dev.path = NULL,
             trace = ifelse(exists(".trace"), .trace, TRUE)) {
        lock = paste0("ask.Y_", id)
        if (!is.null(dev.path)) {
            lock = file.path(dev.path, lock)
        }
        del.file(lock)

        if (trace)
            cat("ask.Y.unlock")
    }

#' Returns matrix which is read in 'X.todo' file (process is stopped if file doesn't exist after a given timeout)
#' @param id suffix of file, should be a unique identifier
#' @param dev.X prefix of file to read (default is 'X.todo')
#' @param timeout tells if 'file' must be removed (default: ~ 1 week)
#' @param clean tells if 'file' must be removed
#'
#' @example while(x <- ask.X()) tell.Y(-sin(x))
#' @example while(x <- ask.X()) tell.Y(-sin(x[,1]*cos(x[,2])))
ask.X <-
    function(id = 0,
             dev.X = "X.todo",
             dev.path = NULL,
             sleep.step = .sleep.step,
             sleep.init = 0.1,
             trace = ifelse(exists(".trace"), .trace, TRUE),
             timeout = .timeout,
             clean = T) {
        if (trace)
            cat("?X ")
        Sys.sleep(sleep.init)
        t = 0
        lock = paste0("ask.X_", id)
        if (!is.null(dev.path)) {
            lock = file.path(dev.path, lock)
        }
        file.create(lock)
        xFile = X.file(
            id = id,
            dev.X = dev.X,
            dev.path = dev.path
        )
        while (!file.exists(file = xFile) & (timeout > 0 & t < timeout)) {
            Sys.sleep(sleep.step)
            t = t + sleep.step
            if (!file.exists(lock)) {
                del.file(xFile)
                stop("ask.X break !")
            }
            if (trace)
                cat(":")
        }
        del.file(lock)
        if (timeout > 0 & t >= timeout) {
            del.file(xFile)
            stop("ask.X timeout !")
        }
#        Sys.sleep(sleep.step)
        if (trace)
            cat(";")
        
        x = read.io(file = xFile, clean = clean)
        
        if (trace)
            cat("(", paste0(collapse = ",", x), ")")
        
        return(x)
    }

ask.X.unlock <-
    function(id = 0,
             dev.path = NULL,
             trace = ifelse(exists(".trace"), .trace, TRUE)) {
        lock = paste0("ask.X_", id)
        if (!is.null(dev.path)) {
            lock = file.path(dev.path, lock)
        }
        del.file(lock)

        if (trace)
            cat("ask.X.unlock")
    }

#' Writes a given matrix to a 'Y.done' file (process is stopped if file already exists after waiting 5 seconds)
#' @param y the object to be written, preferably a matrix or data frame.
#' @param id suffix of file, should be a unique identifier
#' @param dev.Y prefix of file (default is 'Y.done')
# 
tell.Y <-
    function(y,
             id = 0,
             dev.Y = "Y.done",
             dev.path = NULL,
             trace = ifelse(exists(".trace"), .trace, TRUE)) {
        if (trace)
            cat("!Y(", paste0(collapse = ",", y), ") ")
        write.io(y, file = Y.file(
            id = id,
            dev.Y = dev.Y,
            dev.path = dev.path
        ))
    }

#' Write a given matrix to a given file (process is stopped if given file already exists after waiting 5 seconds)
#' @param data the object to be written, preferably a matrix or data frame. If not, it is attempted to coerce 'data' to a data frame.
#' @param file file name where to store matrix
# 
#' @test x=123;write.io(x,"x.dat");read.io("x.dat")
#' @test x=matrix(c(123,456),ncol=2);write.io(x,"x.dat");read.io("x.dat")
#' @test x=matrix(c(123,456,789,101),ncol=2);write.io(x,"x.dat");read.io("x.dat")
write.io <-
    function(data, file, trace = ifelse(exists(".trace"), .trace, TRUE)) {
        i = 0
        while (file.exists(file) &
               i < 100) {
            Sys.sleep(0.05)
            i = i + 1
            if (trace)
                cat(" ")
        }
        if (i >= 100)
            stop("file ", file, " already exists !")
        if (trace)
            cat(">")
        cat(paste0(jsonlite::toJSON(data, na = 'string'), '\n'), file = file)
    }

#' Returns matrix which is read in 'file' (return 'NULL' if file doesn't exist after 0.5 second)
#' @param file name of the file (which constains a matrix representation)
#' @param clean tells if 'file' must be removed
# 
read.io <-
    function(file,
             clean = TRUE,
             trace = ifelse(exists(".trace"), .trace, TRUE)) {
        t = NULL
        
        i = 0
        try(t <- jsonlite::fromJSON(readLines(file)[[1]]), silent = TRUE)
        while (is.null(t) & i < 10) {
            Sys.sleep(0.05)
            i = i + 1
            if (trace)
                cat(" ")
            try(t <- jsonlite::fromJSON(readLines(file)[[1]]), silent = TRUE)
        }
        if (is.null(t) & i < 10 & trace)
            cat("\n:)\n")
        if (clean) {
            file.remove(file)
            if (trace)
                cat("-")
        }
        if (trace)
            cat("<")
        return(t)
    }

#'Build a 'X.todo' file name with a given prefix and given suffix
#' @param id suffix of file, should be a unique identifier
#' @param dev.X prefix of file (default is 'X.todo')
# 
X.file <- function(id = 0,
                   dev.X = "X.todo",
                   dev.path = .default_dev.path) {
    return(tmp.file(id, dev.X, dev.path))
}

#'Build a 'Y.done' file name with a given prefix and given suffix
#' @param id suffix of file, should be a unique identifier
#' @param dev.Y prefix of file (default is 'Y.done')
# 
Y.file <- function(id = 0,
                   dev.Y = "Y.done",
                   dev.path = .default_dev.path) {
    return(tmp.file(id, dev.Y, dev.path))
}

#'Build a 'tmp' file name with a given prefix and given suffix
#' @param id suffix of file, should be a unique identifier
#' @param dev.prefix prefix of file (default is 'Y.done')
# 
tmp.file <- function(id = 0,
                   dev.prefix = "tmp",
                   dev.path = .default_dev.path) {
    if (!is.null(dev.path)) {
        dev.prefix = file.path(dev.path, dev.prefix)
    }
    tmp.file = paste(sep = "_", dev.prefix, id)
    
    return(tmp.file)
}

del.file <- function(file) {
  if (file.exists(file)) {
    try(file.remove(file), silent = TRUE)
  }
}

get.object <- function(file) {
  try(lines <- readLines(file), silent = TRUE)
  if (is.vector(lines) && length(lines) > 0) {
    jsonlite::fromJSON(lines[[1]])
  }
  else {
    NULL
  }
}

set.object <- function(file, data) {
  i <- 0
  while (i < 100) {
    catRes <- try(cat(paste0(jsonlite::toJSON(data, na = 'string'), '\n'), file = file),silent=TRUE)
    if (class(catRes) != "try-error") {
      break
    }
    Sys.sleep(0.05)
    i = i + 1
  }
  if (i >= 100) {
    stop("'set.object' failed, ", catRes$message)
  }
}

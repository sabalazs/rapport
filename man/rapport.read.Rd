\name{rapport.read}
\alias{rapport.read}
\alias{tpl.find}
\title{Read Template}
\usage{
rapport.read(fp, ...)
}
\arguments{
  \item{fp}{a character string containing a template path,
  a template name (for package-bundled templates only),
  template contents separated by newline (\code{\\n}), or a
  character vector with template contents.}

  \item{...}{additional params for header tag matching (see
  \code{\link{grep}})}
}
\value{
a character vector with template contents
}
\description{
Reads file either from template name in system folder, file
path (see \code{rapport.path}) or remote URL, and splits it
into lines for easier handling by \emph{rapport} internal
parser.
}


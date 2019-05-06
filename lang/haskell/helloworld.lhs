\documentclass[12pt]{article}
\usepackage[spanish]{babel}
\usepackage{listings}
\usepackage[latin1]{inputenc}

\lstnewenvironment{code}
{
    \lstset{
        language=Haskell,
        basicstyle=\small,
        frame=single
    }
}
{}

\title{Haskell Hello World}
\author{Luis Cabellos}
\date{}

\begin{document}

\maketitle

\begin{code}
-- Hello World in Haskell
-- by Luis Jose Cabellos
main = print "Hello World"
\end{code}

\end{document}

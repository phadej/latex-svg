Lorem ipsum bla bla something something.
Some text $\sin^2 \theta + \cos^2 \theta = 1$ and $\langle p, q \rangle = ?$
And a bit more text to get paragram

$$
\sum_{x=0}^N x^2
$$

Some other text

$$
1 + 2 + 3 + 4 = 10
$$

More text

$$
\begin{aligned}
& \mathtt{\color{red!50!blue}abab} &&\vdash (\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star \\
& \mathtt{\color{red!50!blue}bab} &&\vdash \mathtt{\color{green!50!black}b}(\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star \\
& \mathtt{\color{red!50!blue}ab} &&\vdash (\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star \\
& \mathtt{\color{red!50!blue}b} &&\vdash \mathtt{\color{green!50!black}b}(\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star \\
&{\color{red!80!black}\varepsilon} &&\vdash (\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star \\
\end{aligned}
$$

And with fix

$$
\begin{aligned}
& \mathtt{\color{red!50!blue}abab} &&\vdash \mathbf{fix}\,{\color{blue}\mathit{x}}={\color{red!80!black}\varepsilon}\cup{\color{blue}\mathit{x}}\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b} \\
& \mathtt{\color{red!50!blue}bab} &&\vdash \mathbf{fix}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}=\mathtt{\color{green!50!black}b}\cup{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b} \\
& \mathtt{\color{red!50!blue}ab} &&\vdash \mathbf{fix}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}ab}}}={\color{red!80!black}\varepsilon}\cup{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}ab}}}\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b} \\
& \mathtt{\color{red!50!blue}b} &&\vdash \mathbf{fix}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aba}}}=\mathtt{\color{green!50!black}b}\cup{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aba}}}\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b} \\
&{\color{red!80!black}\varepsilon} &&\vdash \mathbf{fix}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}abab}}}={\color{red!80!black}\varepsilon}\cup{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}abab}}}\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b} \\
\end{aligned}
$$

Raw LaTeX
---------

\begin{tikzcd}
A \arrow[d, "g"] \arrow[r, "f"] & B \arrow[r, "\alpha"] \arrow[d, "\gamma"] & D \arrow[d, "\beta"] \\
C \arrow[r, "h"] & B' \arrow[r, "\lambda"] & D'
\end{tikzcd}

End

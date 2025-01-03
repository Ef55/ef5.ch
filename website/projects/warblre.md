---
title: Warblre
inlineCSS: >-
  :root {
    --color-rouge: #E30613;
    --color-groseille: #B51F1F;
    --color-leman: #00A79F;
    --color-canard: #007480;
    --color-ardoise: #413D3A;
    --color-perle: #CAC7C7;
    --color-ecmagreen: #2aa198;
    --color-ecmablue: #206CA7;
    --color-ecmayellow: #b58900;
    --color-ecmaorange: #ee8421;
    --color-ecmagray: #cbd7de;
    --hl-literate: #191970;
    --hl-disabled: var(--color-perle);
    --hl-keyword: #B35000;
    --hl-string: var(--color-canard);
    --hl-variable: var(--color-ecmagreen);
  }
  pre, .esh-standalone, .ecma-box {
      font-size: 0.9em;
      line-height: 1.2;
  }
  pre.terminal {
      background: var(--color-ardoise);
      color: white;
      padding: 0.5rem;
      border: inset;
  }
  pre {
      white-space: pre;
  }
  code {
      font-variant-ligatures: none;
      white-space-collapse: preserve;
  }
  code b, code span {
      white-space: pre;
  }
  pre code {
      font-variant-ligatures: initial;
  }
  pre code.coq {
      font-feature-settings: "calt" 0, "COQX" 1;
  }
  code b {
      font-weight: normal;
  }
  code i {
      color: var(--hl-string);
      font-style: inherit;
  }
  code var, code b.v {
      color: var(--hl-variable);
      font-style: italic;
  }
  code .kw {
      color: var(--hl-keyword);
  }
  code .lit {
      color: var(--hl-literate);
      font-family: var(--font-sans);
      font-style: italic;
      margin: 0.35rem 0 0.15rem 0;
      display: inline-block;
  }
  code .lit.off {
      opacity: 0.8;
      margin: 0;
  }
  :is(samp, code) b.ok {
      color: var(--color-leman);
  }
  :is(samp, code) b.warn {
      color: var(--color-ecmayellow);
  }
  #coq pre {
      line-height: 1;
      font-size: 0.85rem;
  }
  #coq code .lit {
      font-size: 0.8rem;
  }
  .esh-raised, .esh-raised-contents, .esh-raised-placeholder, .esh-non-ascii, .esh-non-ascii > span {
    display: inline-block;
    text-indent: 0;
  }
  .esh-non-ascii {
    line-height: 1; /* Work around fonts whose line-height is too large, such as Latin Modern Math */
    width: 1ch;
  }
  .esh-non-ascii > span {
    left: 50%; /* Move to middle of parent */
    position: relative;
  }
  .esh-non-ascii > span > span {
    margin-left: -50%; /* Move back to center in parent */
  }
  .esh-raised-contents {
    font-size: 0.9em;
    width: 0;
  }
  .esh-raised-contents > span {
    position: relative;
  }
  .esh-raised-placeholder {
    height: 0;
    visibility: hidden;
  }
  .esh-standalone {
    line-height: 1;
  }
  .esh-strut {
    display: inline-block;
  }
---

## A mechanization of ECMAScript regexes

The goal of this project was to mechanize in the Coq proof assistant the regexes
defined in the
[ECMA-262](https://ecma-international.org/publications-and-standards/standards/ecma-262/)
standard, the specification follow by the JavaScript language.

The mechanization was designed to be
**auditable**,
**executable**,
**proven-safe**,
and **faithful**.
Since it was written in a proof assistant, it also opens the door to mechanized
proofs about these regexes.


<pre class="esh-standalone" style="font-size:0.8em;">
<code class="coq"><b class="lit"><code>(*&gt;&gt; </code><code>Disjunction</code> :: <code>Alternative</code> | <code>Disjunction</code><code> &lt;&lt;*)</code></b>
| Disjunction <b class="v">r<b class="esh-raised"><b class="esh-raised-contents"><b style="bottom:-0.25em">1</b></b><b class="esh-raised-placeholder">1</b></b></b> <b class="v">r<b class="esh-raised"><b class="esh-raised-contents"><b style="bottom:-0.25em">2</b></b><b class="esh-raised-placeholder">2</b></b></b> <b class="esh-non-ascii"><b><b>⇒</b></b></b>
  <b class="lit"><code>(*&gt;&gt; </code>1. Let <code>m1</code> be <code>CompileSubpattern</code> of <code>Alternative</code> with arguments <code>rer</code> and <code>direction</code>.<code> &lt;&lt;*)</code></b>
  <b class="kw">let</b>! <b class="v">m<b class="esh-raised"><b class="esh-raised-contents"><b style="bottom:-0.25em">1</b></b><b class="esh-raised-placeholder">1</b></b></b> =&lt;&lt; compileSubPattern r<b class="esh-raised"><b class="esh-raised-contents"><b style="bottom:-0.25em">1</b></b><b class="esh-raised-placeholder">1</b></b> (Disjunction_left r<b class="esh-raised"><b class="esh-raised-contents"><b style="bottom:-0.25em">2</b></b><b class="esh-raised-placeholder">2</b></b> <b class="esh-non-ascii"><b><b>∷</b></b></b> ctx) rer direction <b class="kw">in</b>
  <b class="lit"><code>(*&gt;&gt; </code>2. Let <code>m2</code> be <code>CompileSubpattern</code> of <code>Disjunction</code> with arguments <code>rer</code> and <code>direction</code>.<code> &lt;&lt;*)</code></b>
  <b class="kw">let</b>! <b class="v">m<b class="esh-raised"><b class="esh-raised-contents"><b style="bottom:-0.25em">2</b></b><b class="esh-raised-placeholder">2</b></b></b> =&lt;&lt; compileSubPattern r<b class="esh-raised"><b class="esh-raised-contents"><b style="bottom:-0.25em">2</b></b><b class="esh-raised-placeholder">2</b></b> (Disjunction_right r<b class="esh-raised"><b class="esh-raised-contents"><b style="bottom:-0.25em">1</b></b><b class="esh-raised-placeholder">1</b></b> <b class="esh-non-ascii"><b><b>∷</b></b></b> ctx) rer direction <b class="kw">in</b>
  <b class="lit"><code>(*&gt;&gt; </code>3. Return a new <code>Matcher</code> with parameters <code>(x, c)</code> that captures <code>m1</code> and <code>m2</code> and performs
<code>        </code>the following steps when called:<code> &lt;&lt;*)</code></b>
  (<b class="kw esh-non-ascii"><b><b>λ</b></b></b> (<b class="v">x</b>: MatchState) (<b class="v">c</b>: MatcherContinuation) <b class="esh-non-ascii"><b><b>⇒</b></b></b>
    <b class="lit off"><code>(*&gt;&gt; </code>a. Assert: <code>x</code> is a <code>MatchState</code>.<code> &lt;&lt;*)</code></b>
    <b class="lit off"><code>(*&gt;&gt; </code>b. Assert: <code>c</code> is a <code>MatcherContinuation</code>.<code> &lt;&lt;*)</code></b>
    <b class="lit"><code>(*&gt;&gt; </code>c. Let <code>r</code> be <code>m1(x, c)</code>.<code> &lt;&lt;*)</code></b>
    <b class="kw">let</b>! <b class="v">r</b> =&lt;&lt; m<b class="esh-raised"><b class="esh-raised-contents"><b style="bottom:-0.25em">1</b></b><b class="esh-raised-placeholder">1</b></b> x c <b class="kw">in</b>
    <b class="lit"><code>(*&gt;&gt; </code>d. If <code>r</code> is not <code>failure</code>, return <code>r</code>.<code> &lt;&lt;*)</code></b>
    <b class="kw">if</b> r <b class="kw">is</b> not failure <b class="kw">then</b> r
    <b class="lit"><code>(*&gt;&gt; </code>e. Return <code>m2(x, c)</code>.<code> &lt;&lt;*)</code></b>
    <b class="kw">else</b> m<b class="esh-raised"><b class="esh-raised-contents"><b style="bottom:-0.25em">2</b></b><b class="esh-raised-placeholder">2</b></b> x c): Matcher</code>
</pre>

## Links

- Warblre's [repository](https://github.com/epfl-systemf/Warblre);
- The Warblre [paper](https://doi.org/10.1145/3674666);
- The [poster](https://systemf.epfl.ch/posters/2024-warblre/) I presented at the
  [student research competition](https://pldi24.sigplan.org/track/pldi-2024-src)
  @PLDI'24;
- My [Master's
  thesis](https://infoscience.epfl.ch/entities/publication/9b1d1db8-70ad-4735-b6a4-6eccafcaa161),
  even though I'd recommend reading the paper instead;
- My defense [slide deck](https://github.com/Ef55/Reports-and-presentations/blob/main/warblre/slides/build/presentation.pdf).
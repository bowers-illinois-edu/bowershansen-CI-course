# Slide-writing rules — 2026-06-15 (randomization inference / Fisher tea deck)

A consistency guide for revising this deck. Concrete, deck-specific. The aim is
that any two people revising slides make the same choices.

## 1. Bullet style
- One bullet = one idea, stated as a compact **claim**, never a paragraph.
- Default to **sentence fragments**, no trailing period. Drop the subject's
  helping verb: "Random assignment gives known chances" (not "Random assignment
  *is what* gives us known chances").
- Use a **full sentence** only for (a) a definition needing a verb to be
  unambiguous, or (b) a rhetorical question ("How surprising is 4 under no
  discrimination?").
- Target **≤ ~10 words / one line**. Sub-bullets one line.
- A bullet may run to **two lines** only when the idea is genuinely inseparable
  (a defining equation + its gloss, a verbatim quote). Prefer a bullet + indented
  `\item[]` continuation over a wrapped two-line bullet.
- **Precision first**: keep the load-bearing words (no discrimination, fixed,
  known, assignment); cut only connective/filler words.

## 2. Omitting unnecessary words
- **Drop articles** before generic plural/mass nouns when meaning is unchanged:
  "gives known chances", "70 possible assignments".
- **Keep the article** when it points to a specific, already-introduced object:
  "the assignment $\bm z$", "the taster", "the design", "the observed outcome".
  Definiteness carries meaning here.
- **Cut weak/filler verbs and existential frames**: is/are/has/have/there is/
  there are/shows/means/involves.
  - "There are 70 possible assignments" -> "70 possible assignments"
  - "This shows that $t = 4$" -> "$t = 4$"
  - "The test statistic is a rule that..." -> "Test statistic: a rule that..."
- **Prose -> slide**: lead with the concept, push the rest behind a colon or arrow.
  "We summarize the data by counting..." -> "Summary: count milk-first cups judged milk-first".
- **Don't over-compress** into cryptic telegraphs. Keep verbs that carry causal/
  logical force: gives, fixes, holds fixed, recovers, varies.

## 3. Preferred bullet patterns (and when to use each)
- **Concept: unpacking** — "No discrimination: judgments fixed, only $\bm z$ varies."
  (defining/unpacking a term)
- **Claim $\Rightarrow$ consequence** — "Known assignment chances $\Rightarrow$ known distribution of $t$."
  (a logical/causal step; put the consequence on its own line if long)
- **Object $=$ interpretation** — "$t(\bm z,\bm y) = \bm z^\top\bm y$: count of agreements."
  (naming notation)
- **Through-line chain** — "random assignment $\to$ known chances $\to$ reasoned inference."
  (the recurring spine; use at intro and recap)
- Weak -> strong examples:
  - "The probability of a statistic at least as large as 4 equals 1/70."
    -> "$\Pr(t \ge 4) = 1/70$ under no discrimination."
  - "Fisher does not actually rerun the experiment; he imagines other assignments."
    -> "One experiment ran; Fisher varies the assignment in thought."

## 4. Terminology consistency
Use these exact terms, every time:
- **no discrimination** (the claim/model); **perfect discrimination** (the alternative)
- **random assignment** / **randomization** (the process); **assignment** $\bm z$ (a realized one)
- **random variable** $Z_i$; **random vector** $\bm Z$
- **test statistic** $t(\bm z,\bm y)$
- **outcome** = the generic causal term ($y_i$, potential outcomes); **judgment** =
  the lady's call in the tea example. Pick ONE per object on a given slide; never
  alternate judgment/label/response within a slide.
- **known chances** (informal); **randomization distribution** (the distribution of
  $t$ under a model); **error probabilities** (a rule's chances of being wrong)
- Never coin a synonym for an established term mid-deck (no "null" for "no
  discrimination"; no "permutation/sampling distribution" for "randomization distribution").
- **Repetition is good** when it reinforces the spine ("known chances", "reasoned
  basis for inference") across intro -> body -> recap. Repeat anchors on purpose;
  vary only incidental words.

## 5. Emphasis: color, bold, italics
- `\mh{...}` (magenta) = the **one conceptual anchor** of a bullet: the term being
  defined, the key claim word, or the takeaway. <= ~3 highlighted spans per slide,
  1-2 per bullet. Highlight the load-bearing noun/verb ("\mh{no discrimination}",
  "\mh{known chances}", "\mh{fixed}"), not whole clauses.
- `\textbf{...}` (bold) = **structural labels / primitives**: "\textbf{Before}/
  \textbf{After}", "\textbf{Rule}", "\textbf{unit}", "\textbf{randomize}". Bold marks
  role/structure; magenta marks concept. Never bold and highlight the same span.
- `\emph{...}` (italics) = **contrastive stress** on a small word that flips meaning:
  "vary the \emph{assignment}, not the outcomes"; "just \emph{one} sharp model".
- No emphasis for decoration. A slide with zero highlights is fine if nothing is
  the anchor. If everything is highlighted, nothing is.

## 6. Mathematical notation
- **Introduce each symbol once, in words, before reuse**: "$Z_i$ (uppercase): cup
  $i$'s random treatment" before $\bm Z$, $p(\bm z)$, $\Omega$.
- **Gloss every working symbol at first use**: $t(\bm z,\bm y)$ = "a rule applied to
  the assignment and outcomes"; $\bm z^\top\bm y$ = "a count, not a matrix product";
  $\Omega$ = "the assignments that can happen"; $|\Omega|$ = "number of assignments".
- **Never** leave $t(\bm z,\bm y)$, $\bm z^\top\bm y$, $\bm Z$, $\bm Y$ uninterpreted
  on the slide that introduces them.
- **Inline** math in a bullet when short and the point ($\Pr(t\ge4)=1/70$). Use a
  **displayed** equation only when structure matters (the cases form of $Z_i$, the
  $\{0,1\}^N$ set); one display per slide.

## 7. Slide-level structure
- Usually **3-5 top-level bullets**. More than 5 -> two slides.
- A figure/table slide carries **one message in 1-3 bullets**, not a paragraph.
- **One central takeaway** when the slide makes a single move (recap, "Fisher's
  move", "why randomize"): lead with it; sub-bullets support.
- **Split** a slide that mixes two logical moves (define a statistic AND compute it;
  introduce potential outcomes AND use them). One move per slide.
- **Spacing**: keep `\vfill` between items so content distributes; `[t]` for
  build-up lists, default centering for short/figure slides. If a slide looks
  sparse, enlarge the figure or merge — don't pad.

## 8. Pedagogical sequencing
- **Keep a "basic" slide if a later step uses it.** Test: does removing it leave a
  symbol or idea unexplained downstream?
  - $Z_i$, $\bm z$, $\Omega$, $p(\bm z)$ all feed the randomization distribution -> keep.
  - uppercase/lowercase feeds "$\bm y$ fixed, $\bm z$ varies" -> keep.
- Judge by **the student learning the inference logic**, not expert obviousness.
  "Obvious to me" != "already understood by them."
- **Order**: pose the question -> state the value/through-line -> build notation ->
  run the test -> generalize -> recap. Never introduce a rule/threshold before the
  distribution that makes it computable exists.

## 9. Section and subsection titles
- A `\section{}` marks a **major move in the argument** -- one "act" that advances the
  through-line (pose the question -> build the machinery -> run the test -> generalize
  -> recap). Read top-to-bottom, the section titles should be a one-line summary of the
  lecture's logic.
- Title a section by the **move or idea**, never the topic-label or admin tag:
  "Random assignment", "Fisher's exact test", "Potential outcomes", "Recap" --
  not "Part 3", "Definitions", "More results".
- **Parallel and short**: 2-5 words, consistent grammatical form (all noun phrases, or
  all imperatives), Sentence case, no terminal punctuation.
- **New `\section` vs `\subsection`** -- use a new section when ALL of these hold; else
  make it a subsection (or just a slide):
  - the conceptual object or question changes (not a refinement of the current one);
  - you would list it in a one-line outline of the lecture;
  - it spans ~2+ slides forming one coherent step;
  - it is referred to later as a unit ("recall the design").
- Use `\subsection{}` for a refinement, special case, or worked example *within* a
  section's move (e.g. "Fisher's design" under "Random assignment"); typically 1-2
  slides; would not stand alone in the outline.
- **Avoid one-slide sections** -- usually a smell; fold into a broader section. Exception:
  a deliberate standalone beat (a "Recap" can be a one-slide section).
- **Don't over-section.** If two adjacent sections make the same move, merge them. The
  title should answer "what does this step *do*?", not "what is this *about*?".
- A section title may reuse a key term but should not merely duplicate a slide title
  unless the section *is* that single slide (e.g. "Recap").

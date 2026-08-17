# Project notes

## Writing technical reports (curtailment/incident analysis notes)

- Stay factual to the data. These are technical, informative notes — do not
  comment on what changed between versions/drafts of a document (no "this was
  previously missing", "now recovered", etc.); each version states the
  current facts only.
- Use one consistent text format for all written prose within a section — do
  not vary font style/size/colour between the part describing the data
  universe/sample and the part with interpretation. Headings may differ from
  body text; body text itself stays uniform. Bullet lists are fine for an
  actual list (e.g. a recommendations section), not as a way to visually set
  apart "data" from "interpretation" within running prose.
- A curtailment log entry near a fatality date does NOT by itself identify
  the event associated to that fatality. A fatality more typically happens
  when NO curtailment is triggered at all, and the bird is detected already
  inside or very close to the rotor-swept zone (roughly under 100m from the
  turbine). To identify the actual fatality-associated event more precisely,
  analyse the TRACK data (track_dt, not the curtailment log) of the species
  involved, for the relevant days near the turbine, and look for the track(s)
  whose last recorded position ends close to the turbine — a bird observed
  approaching from a few hundred meters, with the track stopping being
  recorded near the turbine, is a stronger collision indicator than a
  curtailment log entry.
- The recorded "incident date" of a fatality is the date the carcass was
  found during a turbine ground survey, NOT the date the bird actually died
  from the collision. Never describe the incident date as "the date of
  mortality", and do not over-focus the analysis on that single day — this
  is exactly why the analysis looks at a window of days before the incident
  (8 days by default in the reports so far; may need to be longer for other
  cases, since survey frequency and carcass detectability vary).

## Communicating about this project (emails, messages)

- All exchanges with Shahin, or about this project generally (emails,
  messages), are written in English — never Portuguese, even though the
  working conversation with Paulo is in Portuguese.
- Write these in Paulo's own voice: a competent non-native English speaker,
  native to Portugal. Natural idiosyncrasies of that voice — slightly formal
  phrasing, direct/literal constructions translated from Portuguese, minor
  non-idiomatic prepositions or word choices — are fine and expected. Do not
  flatten it into polished native-level English, but do not exaggerate it
  into a caricature either; it should read as genuinely natural, not as a
  parody of an accent.

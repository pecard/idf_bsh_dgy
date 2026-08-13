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

---
name: nelson-usage-dates-time
audience: user
modules: time
description: Work with dates, times and durations in Nelson - datetime and duration values, arithmetic, parsing, formatting and ranges. Use when the user handles timestamps, elapsed time, or date ranges.
triggers: date, dates, datetime, heure, time, duree, duration, horodatage, timestamp, calendrier, calendar, jour, day, mois, month, annee, year, semaine, week, parser, parse, formater, format, intervalle, between, posixtime, ecoule, elapsed
functions: datetime, duration, calendarDuration, years, days, hours, minutes, seconds, caldays, calmonths, between, posixtime, weekday, dateshift, isbetween, datenum, datestr, datevec, now, clock
---

# Dates, Times and Durations

Represent instants and spans, do arithmetic on them, and parse or format them correctly.

## When to use
- Creating / parsing timestamps, computing elapsed time, or building date ranges.
- Converting between a `datetime` and numeric/text forms.

## When NOT to use
- Measuring code run time (use `tic`/`toc`).

## Key functions
Instants: `datetime`. Spans: `duration` with `years`/`days`/`hours`/`minutes`/`seconds`, and calendar spans `calendarDuration`/`caldays`/`calmonths`. Relate: `between`, `isbetween`, `weekday`, `dateshift`, `posixtime`. Legacy numeric/text: `datenum`, `datestr`, `datevec`, `now`, `clock`. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. Prefer `datetime`/`duration` over raw serial numbers: the arithmetic and display stay correct and readable.
2. Subtracting two `datetime` values gives a `duration`; add a `duration` to a `datetime` to shift it.
3. Parse text with an explicit input format so ambiguous dates do not misread.
4. Use calendar spans (`caldays`, `calmonths`) when month/day length matters; use fixed `duration` for exact elapsed time.
5. Test membership in a range with `isbetween`; walk to a boundary with `dateshift`.

## Gotchas
- Month-length varies: `calmonths(1)` is not a fixed number of days.
- A `datetime` may carry a time zone; mixing zoned and unzoned values is an error - set the zone deliberately.
- `datestr`/`datenum` are the legacy numeric path; do not mix them with `datetime` arithmetic.

## Done when
The instants/spans are represented with `datetime`/`duration`, arithmetic uses the right (fixed vs calendar) span, and any text parse used an explicit format.

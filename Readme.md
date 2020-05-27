# Patterns

- if connecting two points that have different symmetries,
  then select the symmetry of the first.
  If a point has multiple symmetries defined for it, you
  can keep tapping it to cycle between them.
  Therefore, selection consists of a "point (ref)" and a "option(symmetry id)"


## Actions

- place abs point
- place between 2 points (can edit percent)
- place arc from one around another (can edit theta)

## Buut what if I want symmetries to compound?

I think dots probably should compound.
And shapes maybe too?
So the thing that I was worried about was:
what if I want a thing that's descended from two differently-multiplied points?
Ah yes that's probably the rub. But the first point always carries it.

So then there's not conflict?

Yeah; for points, for lines, for all the things; there's a "source", and that's the point that the other point goes under. Awesome.

So I can restructure things to reflect that.
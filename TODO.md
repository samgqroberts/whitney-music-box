Todos:
- [x] (from Jim's) give each dot a color, probably on a gradient
- [x] (from Jim's) make each dot flash white as it passes the line
- [x] (bug) on the 4 octave chromatic config the smallest 2 dots look misaligned
- [x] (bug) address the background droning noise somehow, seeming to come from multiple notes in our synthesizer playing together
    - looks like the droning tone comes from the lowest note
    - removing all notes but the lowest note causes the drone and only the drone, at all times
    - could it be playing every frame? yep!
    - because the ordinal was 0. ordinals should start at 1
- [ ] punch up visual style (button styling, padding)
- [ ] make it look legit on mobile
- [x] add a few more presets
- [ ] host it

Looser thoughts:
- [ ] flash the note being played ("C#") as that tone passes the line
- [ ] make everything that goes into a ScenarioConfig editable by the user
- [ ] let people create their own note sequences
- [ ] chain different note sequences together, changing the note sequences over time

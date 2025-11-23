# Version Beta 0.3.0
## Changes
### Optimizations
 * Removed ``const&`` from some places where it shouldn't be placed.

### Other
 * We now have shader compilation stage builtin the project.
 * Improved PBR rendering with the help of [Filament](https://google.github.io/filament/Filament.html).
 * Added more test PBR materials.

## New features
 ### Rendering
 Blocks now support individal material per face.

 More tonemapping functions (the default is ``filmic``). TODO: Should make them customizable.
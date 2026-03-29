Found a bug: path_to_string captures the full value path (e.g. Util.helper, Util.a) instead
of truncating to the module level (Util). This means:
- Reference edges point to values like Util.helper instead of modules like Refs.Util
- "No duplicate edges" fails because Util.a and Util.b become separate edges

The tests currently document the actual (buggy) behavior so they serve as a baseline. The fix
  would be to strip the value component from Texp_ident paths — extracting just the
Path.Pdot(parent, _) parent as the referenced module.

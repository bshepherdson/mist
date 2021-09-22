
## Next steps


- Implement the driver input system defined in the DESIGN.md
- Rebuild the Go compiler's output to match it, and to emit the
  bytecodes rather than JSON.
- Comb through the Smalltalk code, replacing <builtin: 'some JS fun'>
  with <primitive: 12>.
  - Implement those primitives.
- Write the bootstrapping classes which are defined in the VM.
- Write the basic dictionary lookups used for symbols (classes, method
  dicts). Ideally that's a full tree-based Dictionary usable from
  Smalltalk, and powered by primitives implemented in the host.
    - Use an AA Tree, simple and balanced.
    - Since symbols are tenured they'll never move.
      - Unless we start compacting the tenured space! Then symbols need either
        a special space of their own or to have the symbol dictionaries rebuilt.

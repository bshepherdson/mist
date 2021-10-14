// GC system!
// Currently does nothing.

// TODO Remember that the class table is roots. They're old->new
// pointers by definition, so they'll always need tracing. Plus creating
// Associations for them during bootstrap is a problem.

// The space is divided into four unequal regions: Tenured, G1, G2 and Eden.
// Currently the scheme divides the available memory into 4MW chunks, and to
// make Tenured, G1 and G2 each 4MW. The rest of the space (eg. 42MW in a 64MW
// address space) is Eden.
//
// - Tenured is permanent, never collected. (For now, eventually it could be
//   subject to mark-and-sweep for a long-running ST image.)
// - G1 and G2 are the old gen and new gen, whose roles swap during major
//   collections.
// - Eden is the new space where objects are allocated. It grows downward, which
//   is handy for checking whether we've overrun because we have a pointer to
//   the head of an object to compare directly against the minimum.
//
// Minor collections happen when we want to allocate something but there's not
// enough room in the Eden for it.
//
// - Shelve the in-progress allocation by saving registers.
// - Scan the GC roots (see below) for pointers into Eden. All reachable Eden
//   values get copied into new gen (one of G1 and G2).
// - Deep scan of the linked list of pointers-of-interest. These are pointers
//   from old spaces (non-Eden) into Eden. They point **at the source in old
//   space**, so it can be updated proprly.
//   - If the source pointer no longer points into Eden, we can just drop this
//     entry from the chain.
//   - Surviving links in Eden get (re)allocated in new gen.
//
// Copy process:
// - Allocate the right size space in new gen, copy the header.
// - Deface the original's header; the pointer to the next space goes at p,
//   and the "GC info" section in p+2 is set to all 1s, to signal it's a
//   redirection.
// - For each pointer in this object, recursively copy it and write the new
//   pointer into the slot in the copy.
//   - `SmallInteger`s can simply be copied.
//   - Likewise non-Eden pointers can be copied.
//
// GC roots:
// - Class table
// - Linked list of oldspace pointers.
// - Active processes and their contexts.



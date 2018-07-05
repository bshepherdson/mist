// Core slices of Javascript that are needed to bootstrap the VM.
//
// Fundamental design: Smalltalk objects are JS objects with a couple of extra
// fields. They don't have JS prototypes. Internal fields are prefixed with $
// since that isn't a legal in ST identifiers.
// These are the fields on an object:
// - $class - Points to the actual class (not its name).
// - $vars - An array of instance variables.
//
// One instance variable on Class is 'methodDict', which is a HashedCollection
// of methods by name; that's the one used for lookup.
//
// Remember the 10 rules of the object system:
//  1. Everything is an object.
//  2. Every object is an instance of a class.
//  3. Every class has a superclass.
//  4. Everything happens by sending messages.
//  5. Method lookup follows the inheritance (superclass) chain.
//  ---------
//  6. Every class is an instance of a metaclass.
//  7. The metaclass hierarchy parallels the class hierarchy.
//  8. Every metaclass inherits from Class and Behavior.
//  9. Every metaclass is an instance of Metaclass.
// 10. The metaclass of Metaclass is an instance of Metaclass.

// Metaclasses are called 'Foo class'.
//
// The core classes are given here:
// - Core hierarchy: ProtoObject -> Object
// - Class hierarchy: (Object) -> Behavior -> ClassDescription -> Class
// - Collections: (Object) -> Collection -> ArrayedCollection -> Array
//                                       -> IndexedCollection -> Dictionary
//                                                            -> HashedCollection

// Class's instance variables: [name superclass instanceVariables classVariable]
const CLASS_VAR_NAME = 0;
const CLASS_VAR_SUPERCLASS = 1;
const CLASS_VAR_INSTVAR_COUNT = 2;
const CLASS_VAR_CLASSVAR_COUNT = 3;

const classes = {};

classes['Metaclass'] = {};
classes['Object class'] = {$class: classes['Metaclass']};
classes['Object'] = {$class: classes['Object_class']};

// Rule 10: The metaclass of Metaclass is an instance of Metaclass.
classes['Metaclass class'] = {$class: classes['Metaclass']};
classes['Metaclass'].$class = classes['Metaclass class'];

// Block closures interact with the VM, so we allow raw construction here.
classes['BlockClosure class'] = {$class: classes['Metaclass']};
classes['BlockClosure'] = {
  $class: classes['BlockClosure class'],
};

const CLOSURE_BYTECODE = 0;
const CLOSURE_ARGV = 1;
const CLOSURE_ARGC = 2;


// Compiled methods are likewise integrated with the VM.
const method = mkInstance(classes['CompiledMethod']);
method.$vars[METHOD_BYTECODE] = ar.bytecode.slice(ar.pc, ar.pc + bc.length);
method.$vars[METHOD_LOCALS] = 1 + bc.argc + bc.temps;
method.$vars[METHOD_ARGC] = bc.argc;
method.$vars[METHOD_SELECTOR] = bc.selector;

// START HERE: Probably these could be refactored into a more properly
// bootstrapped, less manually constructed style. We just need the fundamentals
// of creating subclasses (subclass:) and instances (basicNew), plus Object,
// Behavior, Class and Metaclass.
// Everything else can spread from there, I think.


function mkInstance(cls) {
  return {
    $class: cls,
    $vars: [],
  };
}

Design Decisions 
Our typechecker split the code analysis into two distinct passes, one focusing on classes and the second focusing on expressions.

In the class check, classes are represented as strings. Three hash tables representing parent-child, class-attribute, class-method relationships are loaded with the appropriate information. The information in these hashtables is also used by lub() and is_subtype() for checking class relationships. SELF_TYPE here is symbolically represented as the string “SELF_TYPE” in methods and attributes, to be processed in the expression pass. Issues excluding expressions are handled in this pass, like inheritance cycles, illegal method redefinitions, etc. 

In the expression check, the information loaded into the hash tables is iterated over and expressions are type checked. Classes and types are now mapped to actual static_types from strings. Two hash tables representing object-type and (object, method)-method signature relationships are loaded with previously collected information. The previous symbolic “SELF_TYPE” is added to the object environment, binding self to the actual class it refers to, before expressions are checked. Dispatch, New, and Case handle “SELF_TYPE” similarly, using the binding in the object environment. The lub() functions were used as needed to find common ancestors for branches. There is also a special “Internal” expression kind, used for methods in built-in classes. 

Test Cases

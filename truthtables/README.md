#### Input
| operator | logical statement |
|----------|-------------------|
| `^`      | and               |
| `v`      | or                |
| `->`     | implication       |
| `<=>`    | biconditional     |
| `~`      | not               |

*Note: the `~` should be separated from other operators by a parethesis*
> example: `p^~q` gives an error, while `p^(~q)` is a valid syntax.  
>   `~q^p` is also valid syntax.

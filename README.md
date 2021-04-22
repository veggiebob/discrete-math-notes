#### RSA Encryption.hs
For a certain integer `x` and a certain Encryption known as `En`,
the function `(decryptWith En . encryptWith En)` is an identity
function as long as `n < p * q`

#### Examples
```
> en = createEncryption 13 29    
> en
Encryption { p = 13, q = 29, e = 5, d = 269 }
```
Encryption values `e` and `d` are calculated automagically.
```  
> encryptWith en 1  
1  
> encryptWith en 2  
32  
> encryptWith en 3  
243  
> map (encryptWith en) [1..26]  
[1,32,243,270,109,236,219,346,237,95,72,12,325,222,97,139,75,44,...]
> map (decryptWith en) it  
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26]
```
Notice we got the same values back. It's an identity function!
```  
> ord 'h'  
104  
> chr 104  
'h'  
```
Instead of assigning letters their index in the alphabet, we'll use the ASCII table.
(104 happens to be the same when encrypted)
```
> encryptString en "hello world"  
[104,251,205,205,297,301,214,297,316,205,354]  
> decryptToString en it  
"hello world"
```
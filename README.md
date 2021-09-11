# monad-errors

Experimental error-handling monad transformer which collects errors instead of short-circuiting. Should be reasonably performant. Hopefully doesn't leak space! 

I made this because it annoyed me that "Errors" from Control.Applicative.Lift isn't a monad, but I really wanted a transformer version of it anyway. Figured I'd give it a go and this is what I came up with. 

Note: Satisfaction of typeclass laws in Control.Monad.Errors has not been proven. I think the instances do satisfy the various laws, but I haven't had the time to sit down and go through each of them rigorously. 

(That being said, I've used this somewhat extensively in a few small projects and nothing has broken....yet!)


If you look at the source and get scared by this: 

```
newtype ErrorsT e m a = ErrorsT {unE :: Codensity (m :.: ErrorsK e) (ErrorsK e a)}
```

It might be helpful to point out that that's just an optimized (hopefully!) version of this: 

```
newtype ErrorsT' e m a = ErrorsT' {unE :: m (Errors e a)}
``` 

(I believe, but am not 100% sure, that there's no way to define the relevant functor/applicative/monad instances unless you compose the relevant functors.)

And *that's*  more or less *this*, but with a different applicative instance for `Either` that allows for error collection: 

```
newtype ErrorsT'' e m a = ErrorsT'' {unE :: m (Either e a)} 
```

No idea whether the quantified constraints in the definition of the class are the best way to define it. Seems to work though.


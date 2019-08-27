#![cfg_attr(feature = "nightly", feature(pattern))]
#![warn(missing_docs)]

//! This crate allows you to `Display::fmt` strings that include replacements, without actually doing any replacement until format-time and totally avoiding allocation.
//!
//! This is useful when you do `.replace` and then immediately pass the result to `format!` - it will prevent the intermediate allocation from happening. You can even use the result in another `.lazy_replace` call and it will still avoid allocation, although it may do the inner replacement multiple times. The work of memoizing the result of `Display::fmt` to avoid duplicating work can be done in a generic way by an external crate and requires allocation, so is out of the scope of this crate.

extern crate memchr;

use std::{fmt, ops::Deref};

#[cfg(not(feature = "nightly"))]
pub mod pattern;

#[cfg(feature = "nightly")]
pub use std::str::pattern;

use self::pattern::{Pattern, SearchStep, Searcher};

/// A lazily-replaced string - no work is done until you call `.to_string()` or use `format!`/`write!` and friends. This is useful when, for example, doing `format!("( {} )", my_string.replace(needle, some_replacement)`. Since it uses a `Display` for a replacement, you can even replace a string with a different lazily-replaced string, all without allocating. Of course, this will duplicate work when there is more than one match, but fixing this would require memoization of the `Display` result, which in turn would require allocation. A memoizing `Display` wrapper is out of scope for this crate.
pub struct ReplacedString<'a, P, D> {
    haystack: &'a str,
    needle: P,
    replacement: D,
}

impl<'a, P, D> ReplacedString<'a, P, D> {
    /// Create a struct implementing `Display` that will display the specified string with the specified pattern replaced with the specified replacement
    pub fn new(haystack: &'a str, needle: P, replacement: D) -> Self {
        ReplacedString {
            haystack,
            needle,
            replacement,
        }
    }
}

/// A convenience trait to allow you to call `.lazy_replace` on anything that can deref to a `&str`.
pub trait LazyReplace {
    /// Create a struct implementing `Display` that will display this string with the specified pattern replaced with the specified replacement
    fn lazy_replace<P, D>(&self, pat: P, replacement: D) -> ReplacedString<'_, P, D>;
}

impl<T> LazyReplace for T
where
    T: Deref<Target = str>,
{
    fn lazy_replace<P, D>(&self, needle: P, replacement: D) -> ReplacedString<'_, P, D> {
        ReplacedString {
            needle,
            replacement,
            haystack: &*self,
        }
    }
}

impl<'a, P, D> fmt::Display for ReplacedString<'a, P, D>
where
    P: Pattern<'a> + Clone,
    D: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut searcher = self.needle.clone().into_searcher(self.haystack);
        loop {
            match searcher.next() {
                SearchStep::Match(_, _) => write!(f, "{}", self.replacement)?,
                SearchStep::Reject(start, end) => write!(f, "{}", &self.haystack[start..end])?,
                SearchStep::Done => break,
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::LazyReplace;

    #[test]
    fn it_works() {
        assert_eq!(
            "onetwothree",
            format!("{}", "one!HERE!three".lazy_replace("!HERE!", "two"))
        );
        assert_eq!(
            "onetwothree",
            format!("{}", "onetwo!HERE!".lazy_replace("!HERE!", "three"))
        );
        assert_eq!(
            "onetwothreethree",
            format!("{}", "onetwo!HERE!!HERE!".lazy_replace("!HERE!", "three"))
        );
    }
}

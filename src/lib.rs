#![cfg_attr(feature = "nightly", feature(pattern))]
#![warn(missing_docs)]

//! This crate allows you to `Display::fmt` strings that include replacements, without actually doing any replacement until format-time and totally avoiding allocation.
//!
//! This is useful when you do `.replace` and then immediately pass the result to `format!` - it will prevent the intermediate allocation from happening. You can even use the result in another `.lazy_replace` call and it will still avoid allocation, although it may do the inner replacement multiple times. The work of memoizing the result of `Display::fmt` to avoid duplicating work can be done in a generic way by an external crate and requires allocation, so is out of the scope of this crate.

extern crate memchr;

use std::{
    fmt::{self, Write},
    ops::Deref,
};

#[cfg(not(feature = "nightly"))]
pub mod pattern;

#[cfg(feature = "nightly")]
pub use std::str::pattern;

use self::pattern::{Pattern, SearchStep, Searcher};

/// A type to lazily replace strings in any type that implements `Display`
pub struct ReplaceDisplay<'a, H, R> {
    haystack: H,
    needle: &'a str,
    replacement: R,
}

impl<'a, H, R> ReplaceDisplay<'a, H, R> {
    /// Create a new instance of this type
    pub fn new(haystack: H, needle: &'a str, replacement: R) -> Self {
        ReplaceDisplay {
            haystack,
            needle,
            replacement,
        }
    }
}

impl<'a, D, R> fmt::Display for ReplaceDisplay<'a, D, R>
where
    D: fmt::Display,
    R: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            ReplaceWriter::new(f, self.needle, &self.replacement),
            "{}",
            self.haystack
        )
    }
}

/// A wrapper around a `fmt::Write` that does string replacement on anything that is written to it
/// before passing it to the underlying writer.
pub struct ReplaceWriter<'a, W, R> {
    writer: W,
    needle_pos: usize,
    needle: &'a str,
    replacement: R,
    buffer: String,
}

impl<'a, W, R> ReplaceWriter<'a, W, R>
where
    W: fmt::Write,
    R: fmt::Display,
{
    /// Create a new instance of this type
    pub fn new(writer: W, needle: &'a str, replacement: R) -> Self {
        ReplaceWriter {
            writer,
            needle_pos: 0,
            needle,
            replacement,
            buffer: String::new(),
        }
    }
}

impl<'a, W, R> fmt::Write for ReplaceWriter<'a, W, R>
where
    W: fmt::Write,
    R: fmt::Display,
{
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let rest_needle = &self.needle[self.needle_pos..];

        if s.len() < rest_needle.len() && s.starts_with(&rest_needle[..s.len()]) {
            self.needle_pos += s.len();
            self.buffer.push_str(s);
        } else {
            self.needle_pos = 0;

            if s == rest_needle {
                self.buffer.clear();
                write!(self.writer, "{}", self.replacement)?;
            } else {
                self.writer.write_str(&self.buffer)?;
                self.buffer.clear();

                if let Some(first_char) = self.needle.chars().next() {
                    let mut s = s;

                    while let Some(i) = s.find(first_char) {
                        self.writer.write_str(&s[..i])?;
                        s = &s[i..];

                        let mut len = first_char.len_utf8();
                        let needle_bytes = self.needle.as_bytes();
                        let s_bytes = s.as_bytes();

                        while needle_bytes
                            .get(len)
                            .and_then(|needle| s_bytes.get(len).map(|haystack| haystack == needle))
                            .unwrap_or(false)
                        {
                            len += 1;
                        }

                        if len == self.needle.len() {
                            write!(self.writer, "{}", self.replacement)?;
                            s = &s[len..];
                        } else if len == s.len() {
                            self.buffer.push_str(&s[i..]);
                            self.needle_pos = len;

                            return Ok(());
                        } else {
                            self.writer.write_str(&s[..len])?;
                            s = &s[len..];
                        }
                    }

                    self.writer.write_str(s)?;
                }
            }
        }

        Ok(())
    }
}

/// A lazily-replaced string - no work is done until you call `.to_string()` or use `format!`/`write!` and friends. This is useful when, for example, doing `format!("( {} )", my_string.replace(needle, some_replacement)`. Since it uses a `Display` for a replacement, you can even replace a string with a different lazily-replaced string, all without allocating. Of course, this will duplicate work when there is more than one match, but fixing this would require memoization of the `Display` result, which in turn would require allocation. A memoizing `Display` wrapper is out of scope for this crate.
pub struct ReplacedString<'a, P, R> {
    haystack: &'a str,
    needle: P,
    replacement: R,
}

impl<'a, P, R> ReplacedString<'a, P, R> {
    /// Create a struct implementing `Display` that will display the specified string with the specified pattern replaced with the specified replacement
    pub fn new(haystack: &'a str, needle: P, replacement: R) -> Self {
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
    fn lazy_replace<P, R>(&self, pat: P, replacement: R) -> ReplacedString<'_, P, R>;
}

impl<T> LazyReplace for T
where
    T: Deref<Target = str>,
{
    fn lazy_replace<P, R>(&self, needle: P, replacement: R) -> ReplacedString<'_, P, R> {
        ReplacedString {
            needle,
            replacement,
            haystack: &*self,
        }
    }
}

impl LazyReplace for str {
    fn lazy_replace<P, R>(&self, needle: P, replacement: R) -> ReplacedString<'_, P, R> {
        ReplacedString {
            needle,
            replacement,
            haystack: &*self,
        }
    }
}

/// A convenience trait to allow you to call `.replace_display` on anything that implements `fmt::Display`.
// TODO: Combine with `LazyReplace` once specialisation and GATs are stable
pub trait LazyReplaceDisplay: Sized {
    /// Create a struct implementing `Display` that will display this string with the specified pattern replaced with the specified replacement
    fn replace_display<'a, R>(self, pat: &'a str, replacement: R) -> ReplaceDisplay<'a, Self, R>;
}

impl<T> LazyReplaceDisplay for T
where
    T: fmt::Display,
{
    /// Create a struct implementing `Display` that will display this string with the specified pattern replaced with the specified replacement
    fn replace_display<'a, R>(self, pat: &'a str, replacement: R) -> ReplaceDisplay<'a, Self, R> {
        ReplaceDisplay::new(self, pat, replacement)
    }
}

impl<'a, P, R> fmt::Display for ReplacedString<'a, P, R>
where
    P: Pattern<'a> + Clone,
    R: fmt::Display,
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
    use super::{LazyReplace, LazyReplaceDisplay};

    #[test]
    fn replace_string() {
        assert_eq!(
            "onetwothree",
            "one!HERE!three".lazy_replace("!HERE!", "two").to_string()
        );
        assert_eq!(
            "onetwothree",
            "onetwo!HERE!".lazy_replace("!HERE!", "three").to_string()
        );
        assert_eq!(
            "onetwothreethree",
            "onetwo!HERE!!HERE!"
                .lazy_replace("!HERE!", "three")
                .to_string()
        );
    }

    #[test]
    fn replace_display() {
        assert_eq!(
            "foobar",
            "foo!HERE!".replace_display("!HERE!", "bar").to_string()
        );
        assert_eq!(
            "foobar",
            "!HERE!bar".replace_display("!HERE!", "foo").to_string()
        );
        assert_eq!(
            "foobarbaz",
            format!(
                "{}{}",
                "foo!HERE!".replace_display("!HERE!", "ba"),
                "!HERE!baz".replace_display("!HERE!", "r")
            )
        );
        assert_eq!(
            "fooonetwothreebaz",
            format_args!(
                "{}{}",
                "foo!HERE!".replace_display("!HERE!", "ba"),
                "!HERE!baz".replace_display("!HERE!", "r")
            )
            .replace_display("bar", "one!HERE!three".replace_display("!HERE!", "two"))
            .to_string()
        );
    }
}

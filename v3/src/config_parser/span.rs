use std::ops::{Deref, DerefMut};

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct Spanned<T> {
    start: usize,
    end: usize,
    value: T,
}

impl<T> Spanned<T> {
    pub fn new(start: usize, end: usize, value: T) -> Self { Spanned { start, end, value } }

    pub fn map<TO>(self, mapping_function: impl FnOnce(T) -> TO) -> Spanned<TO> {
        Spanned { start: self.start, end: self.end, value: mapping_function(self.value) }
    }

    pub fn try_map<TO, E>(
        self,
        mapping_function: impl FnOnce(T) -> Result<TO, E>,
    ) -> Result<Spanned<TO>, E> {
        let Spanned { start, end, value } = self;
        mapping_function(value).map(|v| Spanned { start, end, value: v })
    }

    pub fn print_note_and_message_with_context<'a>(
        &self,
        path: &'a str,
        source_contents: &'a str,
        message: &'a str,
        note: &'a str,
    ) -> String {
        // <message>
        //         --> <path>:<lineno>:<offset>
        //          |
        // <lineno> | <contents of line lineno>
        //          |                   ^^^^^^ <note>
        fn find_context<'a>(source_file: &'a str, location: usize) -> (&'a str, String, usize) {
            let mut start = location;
            let mut end = location;

            let mut broke_start = false;
            while &source_file[start..start + 1] != "\n" {
                if start > 0 {
                    start -= 1;
                } else {
                    broke_start = true;
                    break
                }
            }
            if !broke_start && location > start {
                start += 1;
            }

            while &source_file[end..end + 1] != "\n" {
                if end + 1 == source_file.len() {
                    end += 1;
                    break
                } else {
                    end += 1;
                }
            }

            let num_spacing = dbg!(location) - dbg!(start);
            let spacing = " ".repeat(num_spacing);
            let lineno = &source_file[..start + 1].chars().filter(|x| *x == '\n').count() + 1;

            (&source_file[start..end], spacing, lineno)
        }

        let (context_line, spacing, lineno) = find_context(source_contents, self.start);
        let lineno = format!("{}", lineno);

        let mut res = String::new();

        res += message;
        res += "\n";

        res += &" ".repeat(lineno.len());
        res += &format!("--> {}:{}:{}\n", path, lineno, spacing.len());
        res += &" ".repeat(lineno.len() + 1);
        res += "|\n";

        res += &format!("{} |", lineno);
        res += context_line;
        res += "\n";

        res += &" ".repeat(lineno.len() + 1);
        res += "|";
        res += &format!("{}{} {}\n\n", spacing, "^".repeat(self.end - self.start), note);

        res
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target { &self.value }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.value }
}

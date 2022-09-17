#[derive(Clone, Copy)]
struct SeparateTokens<'a>(&'a str);

//impl<'a> Iterator for SeparateTokens<'a> {
//    type Item = &'a str;
//
//    fn next(&mut self) -> Option<Self::Item> {
//        if self.0 == "" {
//            return None;
//        }
//        //if is all uppercase, is only one token
//        if self.0.chars().all(char::is_uppercase) {
//            return Some(std::mem::replace(&mut self.0, ""));
//        }
//
//        let start = if let Some(x) = self.0.find(|x: char| !x.is_uppercase()) {
//            x
//        } else {
//            return Some(std::mem::replace(&mut self.0, ""));
//        };
//        //found a multiple uppercase token
//        //eg PCRel -> pc, rel
//        if start > 1 {
//            let (result, next) = &self.0.split_at(start - 1);
//            self.0 = next;
//            return Some(result);
//        }
//        let end =
//            if let Some(x) = self.0[start..].find(|x: char| x.is_uppercase()) {
//                x
//            } else {
//                return Some(std::mem::replace(&mut self.0, ""));
//            };
//        let (result, next) = &self.0.split_at(end + start);
//        self.0 = next;
//        Some(result)
//    }
//}

pub fn from_sleigh<'a>(input: &'a str) -> impl Iterator<Item = &str> + Clone {
    input
        .split(['.', '_'])
        //separate Snake case into multiple tokens:
        //eg: ArmPCRelImmed12 => [arm, pc, rel, immed12]
        //.map(SeparateTokens)
        //.flatten()
}

pub fn upper_cammel_case<'a, I>(input: I) -> String
where
    I: Iterator<Item = &'a str>,
{
    input
        .map(|ident| {
            let mut iter = ident.chars().map(|ch| ch.to_ascii_lowercase());
            let first = iter.next();
            first
                .map(|first| first.to_ascii_uppercase())
                .into_iter()
                .chain(iter)
        })
        .flatten()
        .collect()
}

pub fn snake_case<'a, I>(input: I) -> String
where
    I: Iterator<Item = &'a str>,
{
    input
        .enumerate()
        .map(|(index, ident)| {
            if index == 0 { "" } else { "_" }
                .chars()
                .chain(ident.chars().map(|ch| ch.to_ascii_lowercase()))
        })
        .flatten()
        .collect()
}

pub fn screaming_snake_case<'a, I>(input: I) -> String
where
    I: Iterator<Item = &'a str>,
{
    input
        .enumerate()
        .map(|(index, ident)| {
            if index == 0 { "" } else { "_" }
                .chars()
                .chain(ident.chars().map(|x| x.to_ascii_uppercase()))
        })
        .flatten()
        .collect()
}

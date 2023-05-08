use std::borrow::Cow;

#[derive(Clone, Copy)]
struct SeparateTokens<'a>(&'a str);

pub fn from_sleigh<'a>(input: &'a str) -> Cow<'a, str> {
    input
        .find('.')
        .map(|_| Cow::Owned(input.replace('.', "_")))
        .unwrap_or(Cow::Borrowed(input))
    //separate Snake case into multiple tokens:
    //eg: ArmPCRelImmed12 => [arm, pc, rel, immed12]
    //.map(SeparateTokens)
    //.flatten()
}

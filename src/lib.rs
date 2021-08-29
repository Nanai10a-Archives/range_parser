use core::fmt::Debug;
use core::ops::Bound;
use core::str::FromStr;

#[allow(dead_code)]
enum Process {
    Before,
    Start,
    Notation,
    End,
}

pub fn parse<N>(src: String) -> (Bound<N>, Bound<N>)
where
    N: Num + FromStr,
    <N as FromStr>::Err: Debug,
{
    let mut status = Process::Before;
    let mut index = 0;

    let mut start: Bound<N> = Bound::Unbounded;
    let mut end: Bound<N> = Bound::Unbounded;

    let mut is_exclude: Option<bool> = None;

    while let Some(c0) = src.chars().nth(index) {
        match c0 {
            ' ' => (),
            '.' => {
                status = match status {
                    Process::Before | Process::Start => Process::Notation,
                    Process::Notation | Process::End => panic!("recognized multiple notation."),
                };

                index += 1;
                match match src.chars().nth(index) {
                    None => panic!("failed parse notation (index: {} | out of range)", index),
                    Some(c) => c,
                } {
                    '.' => (),
                    _ => panic!("failed parse notation (index: {})", index),
                }

                index += 1;
                match src.chars().nth(index) {
                    None => {
                        is_exclude = Some(true);
                        index -= 1;
                    },
                    Some('=') => is_exclude = Some(false),
                    Some(_) => {
                        is_exclude = Some(true);
                        index -= 1;
                    },
                }
            },
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' => {
                status = match status {
                    Process::Before => Process::Start,
                    Process::Notation => Process::End,
                    Process::Start | Process::End =>
                        panic!("unexpected token: '{}' (index: {})", c0, index),
                };

                let mut tmp = String::new();
                tmp.push(c0);

                index += 1;
                while let Some(c1) = src.chars().nth(index) {
                    match c1 {
                        // '_' => (),
                        '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => tmp.push(c1),
                        _ => {
                            index -= 1;
                            break;
                        },
                    }

                    index += 1;
                }

                let num = match tmp.parse() {
                    Ok(o) => o,
                    Err(e) => panic!("parsing error: {:?}", e),
                };

                match (&status, is_exclude) {
                    (Process::Start, None) => start = Bound::Included(num),
                    (Process::Start, Some(_)) => unreachable!(),
                    (Process::End, Some(true)) => end = Bound::Excluded(num),
                    (Process::End, Some(false)) => end = Bound::Included(num),
                    (Process::End, None) => unreachable!(),
                    _ => unreachable!(),
                }

                is_exclude = None;
            },
            _ => panic!("unexpected token: '{}' (index: {})", c0, index),
        }

        index += 1;
    }

    if let Some(false) = is_exclude {
        panic!("`..=` notation must given end bounds.");
    }

    match status {
        Process::Before | Process::Start => panic!("cannot recognized notation."),
        Process::Notation | Process::End => (),
    }

    (start, end)
}

pub trait Num {}
macro_rules! impl_num {
    ($($t:ty)*) => {
        $(
            impl Num for $t {}
        )*
    };
}
impl_num! { usize u8 u16 u32 u64 u128 isize i8 i16 i32 i64 i128 }

#[test]
fn successfully_parse() {
    use Bound::*;

    assert_eq!(parse::<u32>("..".to_string()), (Unbounded, Unbounded));
    assert_eq!(parse::<u32>("0..".to_string()), (Included(0), Unbounded));
    assert_eq!(parse::<u32>("..1".to_string()), (Unbounded, Excluded(1)));
    assert_eq!(parse::<u32>("..=1".to_string()), (Unbounded, Included(1)));
    assert_eq!(parse::<u32>("0..1".to_string()), (Included(0), Excluded(1)));
    assert_eq!(
        parse::<u32>("0..=1".to_string()),
        (Included(0), Included(1))
    );

    assert_eq!(
        parse::<u32>("12345..67890".to_string()),
        (Included(12345), Excluded(67890))
    );
    assert_eq!(
        parse::<u32>("09876..=54321".to_string()),
        (Included(9876), Included(54321))
    );

    assert_eq!(parse::<u32>("   ..   ".to_string()), (Unbounded, Unbounded));
    assert_eq!(
        parse::<u32>("   000   ..   ".to_string()),
        (Included(0), Unbounded)
    );
    assert_eq!(
        parse::<u32>("   ..   0001   ".to_string()),
        (Unbounded, Excluded(1))
    );
    assert_eq!(
        parse::<u32>("   ..=   0001   ".to_string()),
        (Unbounded, Included(1))
    );
    assert_eq!(
        parse::<u32>("   000   ..   0001   ".to_string()),
        (Included(0), Excluded(1))
    );
    assert_eq!(
        parse::<u32>("   000   ..=   0001   ".to_string()),
        (Included(0), Included(1))
    );

    assert_eq!(
        parse::<i32>("-12345..-67890".to_string()),
        (Included(-12345), Excluded(-67890))
    );
    assert_eq!(
        parse::<i32>("-09876..=-54321".to_string()),
        (Included(-9876), Included(-54321))
    );
}

#[test]
#[should_panic(expected = "cannot recognized notation.")]
fn cannot_recognized() { parse::<u32>("   ".to_string()); }

#[test]
#[should_panic(expected = "cannot recognized notation.")]
fn no_notation() { parse::<u32>("   1   ".to_string()); }

#[test]
#[should_panic(expected = "`..=` notation must given end bounds.")]
fn include_notation_wituout_number() { parse::<u32>("   ..=   ".to_string()); }

#[test]
#[should_panic(expected = "failed parse notation (index: 2)")]
fn unexpected_notation() { parse::<u32>("0.1".to_string()); }

#[test]
#[should_panic(expected = "failed parse notation (index: 2 | out of range)")]
fn shortage_notation() { parse::<u32>("0.".to_string()); }

#[test]
#[should_panic(expected = "unexpected token: 'a' (index: 0)")]
fn unexpected_token_0() { parse::<u32>("a".to_string()); }

#[test]
#[should_panic(expected = "unexpected token: 'u' (index: 1)")]
fn unexpected_token_1() { parse::<u32>("0u32..1".to_string()); }

#[test]
#[should_panic(expected = "recognized multiple notation.")]
fn unexpected_token_2() { parse::<u32>("0...1".to_string()); }

#[test]
#[should_panic(expected = "unexpected token: '1' (index: 2)")]
fn unexpected_token_3() { parse::<u32>("0 1..2".to_string()); }

#[test]
#[should_panic(expected = "unexpected token: '2' (index: 5)")]
fn unexpected_token_4() { parse::<u32>("0..1 2".to_string()); }

#[test]
#[should_panic]
fn parsing_error() { parse::<i8>(format!("0..{}", (i16::MAX))); }

#[test]
#[should_panic]
fn parsing_unsigned_from_signed() { parse::<u32>("-1..-2".to_string()); }

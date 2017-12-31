use {SyntaxKind};
use syntax_kinds::*;

use lexer::ptr::Ptr;

pub(crate) fn string_literal_start(c: char, c1: Option<char>, c2: Option<char>) -> bool {
    match (c, c1, c2) {
        ('r', Some('"'), _) |
        ('r', Some('#'), _) |
        ('b', Some('"'), _) |
        ('b', Some('\''), _) |
        ('b', Some('r'), Some('"')) |
        ('b', Some('r'), Some('#')) => true,
        _ => false
    }
}

pub(crate) fn scan_char(ptr: &mut Ptr) {
    if ptr.bump().is_none() {
        return; // TODO: error reporting is upper in the stack
    }
    scan_char_or_byte(ptr);
    if !ptr.next_is('\'') {
        return; // TODO: error reporting
    }
    ptr.bump();
}

pub(crate) fn scan_byte_char_or_string(ptr: &mut Ptr) -> SyntaxKind {
    // unwrapping and not-exhaustive match are ok
    // because of string_literal_start
    let c = ptr.bump().unwrap();
    match c {
        '\'' => {
            scan_byte(ptr);
            CHAR
        }
        '"' => {
            scan_byte_string(ptr);
            CHAR
        }
        'r' => {
            scan_raw_byte_string(ptr);
            CHAR
        }
        _ => unreachable!(),
    }
}

fn scan_byte(ptr: &mut Ptr) {

}

fn scan_byte_string(ptr: &mut Ptr) {

}

fn scan_raw_byte_string(ptr: &mut Ptr) {

}

fn scan_char_or_byte(ptr: &mut Ptr) {
    //FIXME: deal with escape sequencies
    ptr.bump();
}
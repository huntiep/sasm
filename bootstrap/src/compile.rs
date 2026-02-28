/*
enum Ast {
    Ident(Symbol),
    Constant(Value),
    If {
        predicate: Box<Ast>,
        consequent: Box<Ast>,
        alternative: Box<Ast>,
    },
    Application(Box<Ast>, Vec<Ast>),
    Lambda {
        variadic: bool,
        args: Vec<Symbol>,
        body: Vec<Ast>,
    },
    Set {
        ident: Symbol,
        expr: Box<Ast>,
    }
}
*/

pub enum Instruction {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Call,
}

use {Ast, Value};
use {get_symbol, Symbol};
use optimizer::IR;

use std::sync::atomic::{AtomicUsize, Ordering};

fn make_label() -> Symbol {
    static LABEL: AtomicUsize = AtomicUsize::new(0);
    let l = LABEL.fetch_add(1, Ordering::SeqCst).to_string();
    get_symbol(l.into_bytes())
}

fn gen_var() -> Symbol {
    static VAR: AtomicUsize = AtomicUsize::new(0);
    let l = format!("x{}", VAR.fetch_add(1, Ordering::SeqCst));
    get_symbol(l.into_bytes())
}

pub fn compile(exp: Ast) -> Vec<IR> {
    let target = gen_var();
    let mut ir = _compile(exp, target);
    ir.push(IR::Return(target));
    ir
}

fn _compile(exp: Ast, target: Symbol) -> Vec<IR> {
    match exp {
        Ast::Constant(p) => compile_self_evaluating(p, target),
        Ast::Ident(i) => compile_variable(i, target),
        Ast::If { predicate, consequent, alternative } => compile_if(*predicate, *consequent, *alternative, target),
        //Ast::Begin(v) => compile_sequence(v, target),
        Ast::Lambda { variadic, args, body } => compile_lambda(args, body, false, target),
        Ast::Application(op, args) => compile_application(*op, args, target),
        Ast::Set { ident, expr } => compile_set(ident, *expr, target),
        Ast::Define { ident, expr, macrop } => compile_define(ident, *expr, macrop, target),
    }
}

fn compile_self_evaluating(p: Value, target: Symbol) -> Vec<IR> {
    vec![IR::Primitive(target, p)]
}

fn compile_variable(i: Symbol, target: Symbol) -> Vec<IR> {
    vec![IR::Lookup(target, i)]
}

fn compile_define(name: Symbol, expr: Ast, macrop: bool, target: Symbol) -> Vec<IR> {
    let mut n = if let Ast::Lambda { variadic, args, body } = expr {
        compile_lambda(args, body, macrop, target)
    } else {
        _compile(expr, target)
    };
    n.push(IR::Define(name, target));
    n
}

fn compile_set(name: Symbol, expr: Ast, target: Symbol) -> Vec<IR> {
    let mut v = _compile(expr, target);
    v.push(IR::Set(name, target));
    v
}

fn compile_if(pred: Ast, cons: Ast, alt: Ast, target: Symbol) -> Vec<IR> {
    let alt_label = make_label();
    let after_if = make_label();

    /*
    let (pred, cons, alt) = exp.unwrap_if();
    let pred_var = gen_var();
    let mut pred = _compile(pred, pred_var);
    pred.push(IR::GotoIfNot(alt_label, pred_var));

    let cons_var = gen_var();
    let mut cons = _compile(cons, cons_var);
    cons.push(IR::Move(target, cons_var));
    cons.push(IR::Goto(after_if));

    let alt_var = gen_var();
    let mut alt = _compile(alt, alt_var);
    alt.push(IR::Move(target, alt_var));
    alt.insert(0, IR::Label(alt_label));

    pred.push(IR::Phi(target, cons_var, cons, alt_var, alt));
    pred.push(IR::Label(after_if));
    pred
        */
    //let (pred, cons, alt) = exp.unwrap_if();
    let pred_var = gen_var();
    let mut pred = _compile(pred, pred_var);
    pred.push(IR::GotoIfNot(alt_label, pred_var));

    let cons_var = gen_var();
    let mut cons = _compile(cons, cons_var);
    cons.push(IR::Move(target, cons_var));
    cons.push(IR::Goto(after_if));
    cons.push(IR::End(cons_var));
    let cons_begin = IR::Begin(cons_var, cons.len());

    cons.push(IR::Label(alt_label));
    let alt_var = gen_var();
    let mut alt = _compile(alt, alt_var);
    alt.push(IR::Move(target, alt_var));
    alt.push(IR::End(alt_var));
    cons.push(IR::Begin(alt_var, alt.len()));
    cons.append(&mut alt);

    pred.push(IR::Phi(target, cons_var, alt_var, cons.len()));
    pred.push(cons_begin);
    pred.append(&mut cons);
    pred.push(IR::End(target));
    pred.push(IR::Label(after_if));
    pred
}

fn compile_sequence(v: Vec<Ast>, target: Symbol) -> Vec<IR> {
    let mut ir = Vec::new();
    let size = v.len();
    for (i, v) in v.into_iter().enumerate() {
        if i == size - 1 {
            ir.append(&mut _compile(v, target));
        } else {
            ir.append(&mut _compile(v, gen_var()));
        }
    }
    ir
}

fn compile_lambda(args: Vec<Symbol>, body: Vec<Ast>, macrop: bool, target: Symbol) -> Vec<IR> {
    //let (args, body) = exp.unwrap_lambda();
    let ret = gen_var();
    let mut body = compile_sequence(body, ret);
    body.push(IR::Return(ret));
    vec![IR::Fn(target, macrop, args, body)]
}

fn compile_application(op: Ast, mut v: Vec<Ast>, target: Symbol) -> Vec<IR> {
    //let op = v.remove(0);
    //let args = v.len();
    let op_symbol = gen_var();
    let mut ir = _compile(op, op_symbol);
    // TODO: check if macro

    //let mut ir = Vec::new();
    let mut args = Vec::new();
    for arg in v {
        let arg_symbol = gen_var();
        ir.append(&mut _compile(arg, arg_symbol));
        args.push(arg_symbol);
        //ir.push(IR::Param(arg_symbol));
    }
    //ir.append(&mut _compile(op, op_symbol));
    ir.push(IR::Call(target, op_symbol, args));
    ir
}

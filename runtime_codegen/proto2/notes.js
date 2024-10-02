
let bruhh = () => {return "x.val = x.val + 10; y.val = y.val + 10"}

let evalTest = (x, y) =>{
    eval(bruhh())
}

var obj = {val:0}

evalTest(obj, obj)

console.log(obj, bruhh())




// const NIL  = 0
// const CONS = 1

// let nil  = {tag:NIL}
// let cons = (x, xs) => {return {tag:CONS, head:x, tail:xs}}

// var obj1 = {val:0}
// var obj2 = {val:1}
// var list = cons(obj1, cons(obj2, nil))

// let evalTest = (xs, acc) => {
//     switch(xs.tag) {
//     case NIL:


//     case CONS:
// 	return 1
//     }
// }

// console.log("foo" + "mallaccos")

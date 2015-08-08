
function myDelay(func) {
    return [false, func];
}

function myForce(thunk) {
    if(thunk[0]) return thunk[1];
    else {
	thunk[0] = true;
	thunk[1] = thunk[1]();
	return thunk[1];
    }
}

//a silly multiplication example
//What if we wanted to avoid evaluating y unless we really had to?
function myMult(x,y) {
    if(x === 0) return 0;
    else if(x === 1) return y;
    else return y + myMult(x - 1, y);
}

//Here, we only evaluate y if x is not 0. However, we still have to evaluate it for every recursive call
function myMult2(x,yThunk) {
    if(x === 0) return 0;
    else if(x === 1) return yThunk();
    else return yThunk() + myMult2(x - 1, yThunk);
}

//This way, we only evaluate y if x isn't 0 *and* we cache the result of y so we don't have to
//recalculate it for every recursive call.
function myMult3(x, yPromise) {
    if(x === 0) return 0;
    else if(x === 1) return myForce(yPromise);
    else return myForce(yPromise) + myMult3(x - 1, yPromise);
}

//4000
console.log(myMult3(4, myDelay(function(){return 1000;})));

function fibonacci(n) {
    if(n === 0) return 0;
    else if(n === 1) return 1;
    else return fibonacci(n - 1) + fibonacci(n - 2);
}


console.log(myMult2(10, function(){return fibonacci(40);}));//must recalculate fibonacci on every call
console.log(myMult3(10, myDelay(function(){return fibonacci(40);})));//caches the result of fibonacci

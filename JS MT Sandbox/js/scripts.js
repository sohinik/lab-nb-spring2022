function init() {
    // targeting the svg itself
    const svg = document.querySelector("svg");

    // variable for the namespace 
    const svgns = "http://www.w3.org/2000/svg";

    for (let i = 0; i < numCircles; i++) {
        // create svg element for circle
        let newCirc = document.createElementNS(svgns, "circle");

        // randomize circle location and fix aesthetics
        makeCircle(newCirc, getRandomInt(400), getRandomInt(400));

        // add new circle to parent svg
        svg.appendChild(newCirc);
    }

}

function makeCircle(newCirc, x, y) {
    // set attributes using the location from parameters
    newCirc.setAttribute("cx", x);
    newCirc.setAttribute("cy", y);
    newCirc.setAttribute("r", 10);
    newCirc.setAttribute("fill", "#5cceee");

    // return edited circle svg
    return newCirc;
}

function getRandomInt(max) {
    return Math.floor(Math.random() * max);
}

// javascript run on page load
var numCircles = getRandomInt(200);
init();
document.getElementById("demo").innerHTML = numCircles;
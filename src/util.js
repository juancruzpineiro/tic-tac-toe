export function numberToColor(num) {
    switch (num) {
        case 2: return "#F9E57A";
        case 4: return "#C5190E";
        case 8: return "#AF7989";
        case 16: return "#2B2D58";
        case 32: return "#40DFA8";
        case 64: return "#CC1C6A";
        case 128: return "#AA1345";
        case 256: return "#6C94D3";
        case 512: return "#94A517";
        case 1024: return "#2981B3";
        case 2048: return "#F2508A";
        case 4096: return "#F2508A";
        case 8192: return "#A64B81";
        case 16384: return "#A32A8A";
        case 32768: return "#40DAB0";
        case 65536: return "#D1C6E7";
        case 131072: return "#AA8FC3";
        default: return "black";
    }
}

export const equalPos = (posA, posB) => posA.toString() === posB.toString();

export const valueInPos = (pos, grid, numOfColumns) => {
    return grid[pos[0] * numOfColumns + pos[1]];
}

export const posInPath = (pos, path) => {
    return path.some(posI => equalPos(posI, pos));
}

export const connectionInPath = (posA, posB, path) => {
    return path.some((pos, i) => equalPos(pos, posA) && i + 1 < path.length && equalPos(path[i + 1], posB));
}

export const isAdyacent = (posA, posB) => {
    return !equalPos(posA, posB) && Math.abs(posA[0] - posB[0]) <= 1 && Math.abs(posA[1] - posB[1]) <= 1;
}

const smallerPow2GreaterOrEqualThan = (num) => {
    const log2num = Math.floor(Math.log2(num));
    return Math.pow(2, log2num) === num ?  num :Math.pow(2, log2num + 1);
}

export const joinResult = (path, grid, numOfColumns) => smallerPow2GreaterOrEqualThan(path.reduce((result, pos) => result + valueInPos(pos, grid, numOfColumns), 0));



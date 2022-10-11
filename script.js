/* */

const colors = [];

const style_root = getComputedStyle( document.documentElement );
const value = style_root.getPropertyValue( '--tetris-' + color );

const root = document.documentElement;
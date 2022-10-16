/* */


const colors = ['red', 'orange', 'yellow', 'green', 'cyan', 'blue', 'violet', 'magenta'];

const style_root = getComputedStyle( document.documentElement );
//const value = style_root.getPropertyValue( '--tetris-' + color );

const root = document.documentElement;

// MONTA PALETA

const paleta = document.querySelector('.palette');

colors.forEach(color => {

    const swatch = document.createElement('div');
    swatch.classList.add('swatch');
    //swatch.style.backgroundColor = color;
    swatch.style.setProperty('--color', color);

    paleta.appendChild(swatch);

})



// CANVAS

class Question {
    
    // canvas element
    cv;

    // canvas context
    ctx;

    // parameters
    cv_w;
    cv_h;
    smallest;
    sq_side;

    // the squares array
    squares = [];

    // the selected colors array, for this question
    selected_colors = [];

    constructor(question_number) {

        const cv = document.querySelector(`[data-question='${question_number}'] canvas`);
        const ctx = cv.getContext('2d');

        this.cv = cv;
        this.ctx = ctx;

        const cv_w = +window.getComputedStyle(cv).width.slice(0,-2);
        const cv_h = +window.getComputedStyle(cv).height.slice(0,-2);

        this.cv_h = cv_h;
        this.cv_w = cv_w;

        cv.width = cv_w * 2;
        cv.height = cv_h * 2;

        const smallest = Math.min(cv_h, cv_h);

        this.smallest = smallest;

        const sq_side = Math.round(smallest / 24) * 2;

        this.sq_side = sq_side;

        const I = Math.round(cv_w * 2 / sq_side);
        const J = Math.round(cv_h * 2 / sq_side);

        for (let i = 0; i <= I; i++) {

            for (let j = 0; j <= J; j++) {

                const sq = {
                    i : i,
                    j : j,
                    x0 : i * sq_side,
                    y0 : j * sq_side,
                    color : ''
                }

                this.squares.push(sq);

            }

        }

        this.monitor_color_buttons(question_number);

    }

    monitor_color_buttons(question_number) {

        const color_buttons = document.querySelector(`[data-question='${question_number}'] .palette`);

        color_buttons.addEventListener('click', e => this.update_colors(e));

    }

    update_colors(e) {

        e.target.classList.toggle('selected');
    
        if (e.target.classList.contains('swatch')) {
    
            const selected_color = e.target.style.getPropertyValue('--color');
    
            if ( this.selected_colors.includes(selected_color) ) {
    
                this.selected_colors = this.selected_colors.filter(color => color != selected_color)
    
            } else {
                this.selected_colors.push(selected_color);
            }
    
        }
    
        this.set_colors(this.selected_colors);
    
    }

    set_colors(color_list) {

        const n = color_list.length;
        const sq_side = this.sq_side;
    
        this.squares.forEach(sq => {
    
            const color = n == 0 ? 'white' : color_list[ (sq.i + sq.j) % n ];
    
            this.ctx.fillStyle = color;
            this.ctx.fillRect(sq.x0, sq.y0, sq_side, sq_side);
            this.ctx.fill();
    
            //console.log(sq.i, sq.j, (sq.i + sq.j) % n , color)
    
        });
    
    }

}

const question01 = new Question('01');








// URL

let [frm_nationality, frm_age, frm_music_xp, frm_music_listening_habits, frm_song1_colors, frm_song2_colors, frm_song3_colors] =['Brasil', '42', 'a', 'd', '341', 3, 2];

let api_url = 'http://137.184.187.148/api/save?'

api_url += 'nat=' + frm_nationality;
api_url += '&age=' + frm_age;
api_url += '&mxp=' + frm_music_xp;
api_url += '&mls=' + frm_music_listening_habits;
api_url += '&so1=' + frm_song1_colors;
api_url += '&so2=' + frm_song2_colors;
api_url += '&so3=' + frm_song3_colors;

console.log(api_url);



// SUBMIT BUTTON

const btn_submit = document.querySelector('button[type="submit"]');
function send() {
    console.log('will send')
    fetch(api_url, {mode: 'no-cors'}).then(console.log("sent"));
}

btn_submit.addEventListener('click', send)
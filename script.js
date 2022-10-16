/* */


const colors = ['red', 'orange', 'yellow', 'green', 'cyan', 'blue', 'violet', 'magenta'];

const style_root = getComputedStyle( document.documentElement );
//const value = style_root.getPropertyValue( '--tetris-' + color );

const root = document.documentElement;

// CANVAS

class Question {

    //
    question_number
    
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

        this.question_number = question_number;

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

        this.build_palette(question_number);

        this.monitor_color_buttons(question_number);

    }

    build_palette(question_number) {

        const palette = document.querySelector(`[data-question='${question_number}'] .palette`);

        colors.forEach(color => {

            const swatch = document.createElement('div');
            swatch.classList.add('swatch');
            //swatch.style.backgroundColor = color;
            swatch.style.setProperty('--color', color);

            palette.appendChild(swatch);

        })

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

    get_colors_string() {

        if (this.selected_colors.length > 0) {
            return this.selected_colors.reduce( (ac, cv) => ac + ',' + cv, this.question_number)
        }
        else {
            return 'empty'
        }
        
    }

}

const question01 = new Question('01');
const question02 = new Question('02');
const question03 = new Question('03');
const question04 = new Question('04');
const question05 = new Question('05');
const question06 = new Question('06');

// FORM DATA

function get_radio_value(name) {

    let vl;

    var buttons = document.getElementsByName(name);

    buttons.forEach(button => {
        console.log(button, button.checked, button.value);
        if (button.checked) {
            vl = button.value
        }
    })

    return vl;
          
}

const age = document.querySelector('#form-age').value;
const nationality = document.querySelector('#form-nationality').value;
const gender = get_radio_value('gender');
const instrument = get_radio_value('instrumentyn');
const instrument_text = document.querySelector('#form-which-instrument').value;
const music_xp = get_radio_value('musicexperienceyn');
const music_xp_text = document.querySelector('#form-music-experience-text').value;
const genres = document.querySelector('#form-music-genres').value;

const colors_q01 = question01.selected_colors;

function validate() {

    console.log(age, nationality, gender, instrument, instrument_text, music_xp, music_xp_text, genres);


}








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
/*
const btn_submit = document.querySelector('button[type="submit"]');
function send() {
    console.log('will send')
    fetch(api_url, {mode: 'no-cors'}).then(console.log("sent"));
}

btn_submit.addEventListener('click', send)*/
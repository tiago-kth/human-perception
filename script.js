/* */

// language
const lang = document.documentElement.getAttribute('lang')


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

    // the arcs array
    arcs = [];
    R;

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

        // arcs 

        const R = Math.sqrt( Math.pow(cv_h * 2, 2) + Math.pow(cv_w * 2, 2) );
        this.R = R;

        const arc_count = 32;
        const theta_inc = Math.PI * 2 / arc_count;

        for (let i = 0; i < arc_count; i++) {

            const theta_i = theta_inc * i;
            const theta_f = theta_inc * i + theta_inc;

            const xi = Math.cos(theta_i) * R + cv_w;
            const xf = Math.cos(theta_f) * R + cv_w;

            const yi = Math.sin(theta_i) * R + cv_h;
            const yf = Math.sin(theta_f) * R + cv_h;

            const arc = {i, theta_i, theta_f, xi, yi, xf, yf};

            this.arcs.push(arc);

        }

        //

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

    set_colors_radial(color_list) {

        const n = color_list.length;

        this.arcs.forEach( arc => {

            const color = n == 0 ? 'white' : color_list[ arc.i % n ];

            this.ctx.fillStyle = color;
            this.ctx.beginPath();
            this.ctx.moveTo(this.cv_w, this.cv_h);
            this.ctx.lineTo(arc.xi, arc.yi);
            this.ctx.arc(this.cv_w, this.cv_h, this.R, arc.theta_i, arc.theta_f, false);
            this.ctx.lineTo(this.cv_w, this.cv_h);
            this.ctx.closePath();
            this.ctx.stroke();
            this.ctx.fill();


        })

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

const question_numbers = ['01', '02', '03', '04', '05', '06'];
const questions = question_numbers.map(question_number => new Question(question_number));

/*
const question01 = new Question('01');
const question02 = new Question('02');
const question03 = new Question('03');
const question04 = new Question('04');
const question05 = new Question('05');
const question06 = new Question('06');
*/

// FIRST PAGE

class FirstPage {

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

    colors;

    I;
    J;

    constructor(colors) {

        this.colors = colors;


        const cv = document.querySelector('.first-page canvas');
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

        this.I = I;
        this.J = J;

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

        this.set_colors();

    }

    set_colors() {

        const n = this.colors.length;
        const sq_side = this.sq_side;
    
        this.squares.forEach(sq => {

            const dice = Math.random();
            //let dice = perlin.get(sq.i / this.I, sq.j / this.J);
            //dice = dice / 2 + 0.5

            let color;

            const white_probability = 0.4;

            if (dice < white_probability) color = 'white';
            else {

                const index = Math.round( ( (1 - dice) / (1 - white_probability) ) * n  );

                color = this.colors[index];

            }
    
            this.ctx.fillStyle = color;
            this.ctx.fillRect(sq.x0, sq.y0, sq_side, sq_side);
            this.ctx.fill();
    
            //console.log(sq.i, sq.j, (sq.i + sq.j) % n , color)
    
        });
    
    }

}

const opening = new FirstPage(colors);




// FORM DATA

function get_radio_value(name) {

    let vl;

    var buttons = document.getElementsByName(name);

    buttons.forEach(button => {
        //console.log(button, button.checked, button.value);
        if (button.checked) {
            vl = button.value
        }
    })

    return vl;
          
}

function validate() {

    let age = document.querySelector('#form-age').value;
    let nationality = document.querySelector('#form-nationality').value;
    let gender = get_radio_value('gender');
    let instrument = get_radio_value('instrumentyn');
    let instrument_text = document.querySelector('#form-which-instrument').value;
    let music_xp = get_radio_value('musicexperienceyn');
    let music_xp_text = document.querySelector('#form-music-experience-text').value;
    let genres = document.querySelector('#form-music-genres').value;

    let errors = [];

    if (age == '') errors.push(lang == 'en' ? 'age' : 'idade');
    if (nationality == '') errors.push(lang == 'en' ? 'nationality' : 'nacionalidade');
    if (genres == '') errors.push(lang == 'en' ? 'music genres' : 'gêneros musicais');

    questions.forEach(question => {
        if (question.get_colors_string() == 'empty') {
            errors.push( (lang == 'en' ? 'color selection for excerpt ' : 'seleção de cores para o trecho ') + question.question_number)
        }
    })

    const colors_string = questions
      .map(q => q.get_colors_string())
      .reduce( (pr, cv) => pr + ' ' + cv);

    //console.log(colors_string);

    const p_msg = document.querySelector('p.errors-text');

    //console.log(errors);
    if (errors.length > 0) {

        const errors_text = errors.reduce((pr, cv) => pr + ', ' + cv);
        p_msg.innerHTML = ( lang == 'en' ? 'Hmm... some answers are missing. Please check: ' : 'Hmm... parece que algumas respostas estão falando. Por favor, verifique: ' ) + errors_text;
        return 'error';

    } else {

        const string = 
            'age=' + age +
            '&nat=' + nationality +
            '&gnd=' + gender +
            '&ins=' + instrument +
            '&instxt=' + instrument_text +
            '&mxp=' + music_xp +
            '&mxptext=' + music_xp_text +
            '&genres=' + genres +
            '&colors=' + colors_string
        ;

        //console.log(nationality, string);
        return string;

    }

}








// URL

//let [frm_nationality, frm_age, frm_music_xp, frm_music_listening_habits, frm_song1_colors, frm_song2_colors, frm_song3_colors] =['Brasil', '42', 'a', 'd', '341', 3, 2];

let api_url = 'https://tiago.fyi/api/save?' //'http://127.0.0.1:8000/save?'

// SUBMIT BUTTON

const btn_submit = document.querySelector('button');
const bk_link = document.querySelector('.back-link');
const p_msg = document.querySelector('p.errors-text');

function send() {

    p_msg.dataset.status = "validating";

    p_msg.innerHTML = lang == 'en' ? "Validating..." : "Validando..."

    bk_link.classList.add('hidden');

    let ans = validate();

    if (ans == 'error') {

        p_msg.dataset.status = "error";
        bk_link.classList.remove('hidden');
        //console.log(ans);

    } else {

        api_url += ans;

        //console.log('will send: ', api_url);
        p_msg.dataset.status = "sending";

        p_msg.innerHTML = lang == 'en' ? "Validated! Sending..." : "Validado! Enviando..."

        fetch(api_url, {mode: 'no-cors'}).then(() => {
            //console.log("sent");
            
            p_msg.innerHTML = lang == 'en' ? 
                "Sent! Thank you very much for your participation! If you liked the songs, you can listen to <a href='https://www.youtube.com/watch?v=WEc2KGBMogg'>this full album on Youtube.</a> :)" :
                "Enviado! Muito obrigado por sua participação! Você pode ouvir mais Chorinho <a href='https://www.youtube.com/watch?v=WEc2KGBMogg'>aqui neste link do Youtube</a>." ;
            p_msg.dataset.status = "sent";
            btn_submit.disabled = true;
            
        });

    }
    
}

btn_submit.addEventListener('click', send);

function back_link_clicked() {

    p_msg.innerHTML = '';
    p_msg.dataset.status = "unsent";
    bk_link.classList.add('hidden');
    
}

bk_link.addEventListener('click', back_link_clicked);

console.log('Hej! Thank you for visiting! :) This was done using only vanilla HTML / CSS / JS. You can check the repository at: https://github.com/tiago-kth/human-perception/')
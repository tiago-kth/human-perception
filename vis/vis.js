const number = new Intl.NumberFormat('en', {maximumFractionDigits: 2, minimumFractionDigits: 2});

function unique(arr) {

    return arr.filter( (d, i, a) => a.indexOf(d) == i );

}

class Chart {

    w;
    h;

    factor = 2;

    cv;
    ctx;

    constructor(ref) {

        const cont = document.querySelector('.container-' + ref);
        const cv = document.querySelector('.' + ref);
        this.cv = cv;
        this.ctx = cv.getContext('2d');

        const w = +window.getComputedStyle(cont).width.slice(0,-2);
        const h = +window.getComputedStyle(cont).height.slice(0,-2);

        [this.w, this.h] = [this.factor * w, this.factor * h];

        cv.width = this.factor * w;
        cv.height = this.factor * h;

        console.log(this.w, this.h);

    }

}

class PlotGrid {

    ncol; 
    nrow;

    margin;

    h_cell;
    w_cell;

    params = [];

    grids = [];

    constructor(chart, ncol, nrow, margin) {

        this.ncol = ncol;
        this.nrow = nrow;
        this.margin = margin;

        const w = chart.w;
        const h = chart.h;

        const w_side = w / ncol;
        const h_side = h / nrow;

        this.h_cell = h_side;
        this.w_cell = w_side;

        const n_grids = ncol * nrow;

        for (let k = 0; k < n_grids; k++) {

            const i = k % ncol;
            const j = Math.floor(k / ncol);

            this.params.push({
                x0 : i * (w_side) + margin,
                y0 : j * (h_side) + margin
            })

        }

    }

}



fetch('unit-chart.json')
  .then(response => response.json())
  .then(data => {

    const excerpts_list = unique(data.map(d => d.excerpt));

    function get_excerpts_count(data) {

        console.log(excerpts_list);

        const answ = {};

        excerpts_list.forEach(excerpt => {

            answ[excerpt] = data.filter(d => d.excerpt == excerpt).length;

        })

        return(answ);

    }

    const excerpts_count = get_excerpts_count(data);

    const chart = new Chart('chart');

    const plot_grid = new PlotGrid(chart, 3, 2, 60);
    console.log(plot_grid, chart);

    function get_square_size(data, w, h, margin) {

        const max_count = Math.max(...Object.values(excerpts_count));

        const W = w - 2*margin;
        const H = h - 2*margin;

        const min = Math.min(W,H);
        console.log(min);

        const area = min * min;

        const n = max_count;

        const sq_outer_area = area / n;
        
        const sq_outer_side = Math.sqrt(sq_outer_area);

        console.log(sq_outer_area, n, area, sq_outer_side);

        return sq_outer_side

    }

    const sq_outer_side = get_square_size(data, plot_grid.w_cell, plot_grid.h_cell, plot_grid.margin);
    console.log(sq_outer_side);

    function make_grid_excerpt(excerpt) {

        const mini_data = data.filter(d => d.excerpt == excerpt);

        const n = mini_data.length;

        const n_side = Math.ceil(Math.sqrt(n));
        console.log(n, n_side);

        const grid = [];

        mini_data.forEach(d => {

            const j = Math.floor( (d.k - 1) / n_side);
            const i = (d.k - 1) % n_side;

            const color = d.color;

            grid.push({i, j, color});

        })

        return grid;

    }

    function make_grid_dims(grid, x0, y0, gap) {

        return grid.map(sq => (
            {
                x0 : x0 + sq_outer_side * sq.i,
                y0 : y0 + sq_outer_side * sq.j,
                w : sq_outer_side - gap,
                h : sq_outer_side - gap,
                color : sq.color
            }
        ))

    }

    function render_grid(grid, chart) {

        const ctx = chart.ctx;

        grid.forEach(sq => {

            ctx.fillStyle = sq.color;
            ctx.fillRect(sq.x0, sq.y0, sq.w, sq.h);
            ctx.fill();

        })

    }

    const excerpts_list_order = ['06', '05', '02', '01', '04', '03'];
    const labels = [];

    excerpts_list_order.forEach((excerpt,i) => {

        grid_params = plot_grid.params[i];

        let grid = make_grid_excerpt(excerpt);
        grid = make_grid_dims(grid, grid_params.x0, grid_params.y0, 4);

        plot_grid.grids = [...plot_grid.grids, ...grid];


        const sample = data.filter(d => d.excerpt == excerpt)[0];
        const name = sample.name;
        const expression = sample.expression;
        const count = excerpts_count[excerpt];
        const x0 = grid_params.x0 / chart.factor;
        const y0 = grid_params.y0 / chart.factor;

        labels.push({name, expression, count, x0, y0});

        // chart.ctx.strokeStyle = 'blue';
        // chart.ctx.lineWidth = 20;
        // chart.ctx.fillRect(grid_params.x0, grid_params.y0, grid_params.w_cell, grid_params.h_cell);
        // chart.ctx.stroke();


    })

    console.log(labels);

    //const grid_01 = make_grid_excerpt('01');
    //const grid_01_dims = make_grid_dims(grid_01, 0, 0, 2);
    render_grid(plot_grid.grids, chart);

    function render_labels(labels) {

        const x0_chart = chart.cv.getBoundingClientRect().x;
        const y0_chart = chart.cv.getBoundingClientRect().y;

        console.log(x0_chart, y0_chart);

        const cont = document.querySelector('body');

        labels.forEach(label => {

            const div = document.createElement('div');
            div.classList.add('label-container');

            const p_nome = document.createElement('h2');
            p_nome.classList.add('label-title');
            p_nome.innerHTML = label.name + ' (' + label.expression + ')';

            const p_count = document.createElement('p');
            p_count.classList.add('label-count');
            p_count.innerHTML = `${label.count} colors (average: ${number.format(label.count / 206)} per participant)`;

            div.appendChild(p_nome);
            div.appendChild(p_count);

            div.style.top = (label.y0 + y0_chart) + 'px';
            div.style.left = (label.x0 + x0_chart) +  'px';

            cont.append(div);


        })

    }

    render_labels(labels);

    //console.log(grid_01, grid_01_dims);


})
function unique(arr) {

    return arr.filter( (d, i, a) => a.indexOf(d) == i );

}

class Chart {

    w;
    h;

    cv;
    ctx;

    constructor(ref) {

        const cont = document.querySelector('.container-' + ref);
        const cv = document.querySelector('.' + ref);
        this.cv = cv;
        this.ctx = cv.getContext('2d');

        const w = +window.getComputedStyle(cont).width.slice(0,-2);
        const h = +window.getComputedStyle(cont).height.slice(0,-2);

        [this.w, this.h] = [2 * w, 2 * h];

        cv.width = 2 * w;
        cv.height = 2 * h;

        console.log(this.w, this.h);

        //svg.width = w;
        //svg.height = h;

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

    const chart = new Chart('chart');

    let [w, h, margin] = [chart.w, chart.h, 20];

    function get_grid_parameters(data, w, h, margin) {

        const excerpts_count = get_excerpts_count(data);
        const max_count = Math.max(...Object.values(excerpts_count));

        const gap = 5;

        const W = w - 2*margin;
        const H = h - 2*margin;

        const min = Math.min(W,H);
        console.log(min);

        const area = min * min;

        const n = max_count;

        const sq_outer_area = area / n;
        
        const sq_outer_side = Math.sqrt(sq_outer_area);

        const n_side = Math.ceil( min / sq_outer_side);

        console.log(sq_outer_area, n, area, sq_outer_side);

        return sq_outer_side

    }

    const sq_outer_side = get_grid_parameters(data, w, h, margin);
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

    function plot_grid(grid, chart) {

        const ctx = chart.ctx;

        grid.forEach(sq => {

            ctx.fillStyle = sq.color;
            ctx.fillRect(sq.x0, sq.y0, sq.w, sq.h);
            ctx.fill();

        })

    }

    const grid_01 = make_grid_excerpt('01', chart.w * 2, chart.h * 2, 20);
    const grid_01_dims = make_grid_dims(grid_01, 0, 0, 2);
    plot_grid(grid_01_dims, chart);

    console.log(grid_01, grid_01_dims);





})
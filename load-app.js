import show_error from './show-error.mjs';
import observer from './svg-touches.mjs';

async function init_app() {
    const compilation_error = await show_error;
    if(compilation_error) {
        return;
    }
    const container = document.createElement('div');
    document.body.innerHTML = '';
    document.body.appendChild(container);
    const app = Elm.DoubleBack.init({node: container, flags: {}});

    // New event listener:
    window.addEventListener("load",function() {
        setTimeout(function(){
            // Hide the address bar:
            window.scrollTo(0, 1);
        }, 0);
    });
}

init_app();

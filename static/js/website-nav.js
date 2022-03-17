// hamburger expando for navbar on a generic page
function activateNavbarBurger() {
    const nav = document.querySelector('#main-navbar');
    const burger = nav.querySelector('.navbar-burger');
    const menu = nav.querySelector('.navbar-menu')
    burger.addEventListener('click', () => {
        burger.classList.toggle('is-active');
        menu.classList.toggle('is-active');
    });
}

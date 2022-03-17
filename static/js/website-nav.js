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

// tab structure on index page

function toggleTabNav(obj) {
    let tabs = document.getElementById('nav-tabs');
    let activeTabs = tabs.getElementsByClassName('is-active');
    Array.prototype.forEach.call(activeTabs, function(tab) {
        tab.classList.remove('is-active');
    }
    );
    let targetTab = obj.getAttribute('data-tab-nav');
    obj.parentNode.classList.add('is-active');
    let targetObj = document.getElementById('tab-' + targetTab);

    let tabview = document.getElementById('tabview');
    let allTabContents = tabview.getElementsByClassName('content-tab')
    Array.prototype.forEach.call(allTabContents, function(tab) {
        tab.classList.add('is-hidden');
    }
    );
    if(targetTab === "home") {
        window.location.hash = "";
    } else {
        window.location.hash = "#!" + targetTab;
    }
    targetObj.classList.remove('is-hidden');
}

// allow linking to tabs using #!tab-name
function handleFragment() {
    let tabs = document.getElementById('nav-tabs');
    let activeTabName;
    if(window.location.hash) {
        activeTabName = window.location.hash.substring(2);
    } else {
        activeTabName = "home";
    }
    let activeTab = tabs.querySelector('a[data-tab-nav=' + activeTabName + ']');
    toggleTabNav(activeTab);
}

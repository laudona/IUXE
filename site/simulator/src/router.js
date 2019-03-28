import Vue from 'vue'
import Router from 'vue-router'
import TwoPane from './views/TwoPane'

Vue.use(Router)

export default new Router({
  routes: [
    {
      path: '/',
      name: 'home',
      component: TwoPane
    }
  ]
})

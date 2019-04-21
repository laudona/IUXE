import Vue from 'vue'
import Vuetify from 'vuetify/lib'
import 'vuetify/src/stylus/app.styl'

Vue.use(Vuetify, {
  iconfont: 'md',
  theme: {
    primary: '#F76201',  // Burnt Orange
    secondary: '#D41E00', // Scarlet
    accent: '#FEEBA0', // Mimosa
    error: '#FF5252',
    info: '#672901', // Burnt Sienna
    success: '#4CAF50',
    warning: '#FFC107'
  }
})


<template>
  <div>
    <playing-screen :enabled="playing" @finished="playingFinished"></playing-screen>
    <vs-button vs-type="filled" @click="playing = true" size="100px" radius>Start</vs-button>
  </div>
</template>
<script>
  import PlayingScreen from './PlayingScreen.vue';
  export default {
        name: "bingo-app",
        data: () => ({
          playing: false,
        }),
        components: {
          PlayingScreen,
        },
        methods: {
          playingFinished() {
            this.playing = false;
          }
        },
    }
</script>
<style lang="scss" scoped>
  
</style>
<style lang="scss">
  .vs-button--text {
    font-size: 1.5em;
    font-weight: 600;
  }
</style>

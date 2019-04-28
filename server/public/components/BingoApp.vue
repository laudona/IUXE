<template>
  <div>
    <playing-screen :enabled="playing" @finished="playingFinished"></playing-screen>
    <vs-button vs-type="filled" @click="playing = true">Start</vs-button>
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
<template>
  <div>
    <!-- https://tobiasahlin.com/moving-letters/#4 -->
    <!-- https://daneden.github.io/animate.css/ -->
    <vs-row vs-align="center">
      <div class="count">
        {{ toCount }}
      </div>
    </vs-row>
    <vs-row>
      <vs-col vs-type="flex" vs-w="10" vs-offset="1">
        <vs-progress :percent="progress" :height="24" color="primary">primary</vs-progress>
      </vs-col>
    </vs-row>
  </div>
</template>
<script>
export default {
    name: "playing-screen",
    props: {
      enabled: Boolean,
    },
    data: () => ({
      toCount: 10,
      totalTime: 10,
    }),
    computed: {
      progress() {
        return (this.totalTime - this.toCount) / this.totalTime * 100;
      }
    },
    methods: {
      startTimer(seconds) {
        this.toCount = this.totalTime = seconds;
        this.playing = true;
        this.interval = setInterval(() => {
        this.toCount--;
        if (this.toCount <= 0) {
          clearInterval(this.interval);
          this.playing = false;
          this.$emit('finished');
        }
      }, 1000);
      }
    },
    watch: {
      enabled(val) {
        if (val && !this.playing) {
          this.startTimer(10);
        }
      }
    },
    mounted() {
      this.startTimer(10);
    },
  }
</script>
<style lang="scss" scoped>
  span {
    background-color: green;
  }
  .count {
    font-family: 'Roboto';
    font-size: 25rem;
    width: auto;
    margin: 0 auto;
  }
</style>
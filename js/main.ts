import './bootstrap';
import './corelib';
import './bytecodes';
import './primitives';
import {DebugDriver, Driver, WordStream} from './driver';
import './debug';
import {frameLoop, initEvents} from './interrupts';
import {vm} from './vm';

function main() {
  fetch('/testing.bin').then(resp => resp.arrayBuffer()).then(buf => {
    const input = new Uint16Array(buf);
    const stream = new WordStream(input);
    const driver = new Driver(stream);

    window.__canvas = document.getElementById('ui') as HTMLCanvasElement;
    window.__ui = window.__canvas.getContext('2d')!;

    const debug = new DebugDriver(new WordStream(input)).allCommands();
    while (!stream.atEnd()) {
      driver.interpret();
    }

    vm.bootstrapComplete = true;
    initEvents(window.__canvas);
    frameLoop();
  });
}

main();


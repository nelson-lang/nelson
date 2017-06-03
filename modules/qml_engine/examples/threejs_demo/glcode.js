// glcode.js
Qt.include("three.js")
var camera, scene, renderer;
var cube;
var mesh;
function initializeGL(canvas) {
    camera = new THREE.PerspectiveCamera( 27, canvas.innerWidth / canvas.innerHeight, 1, 4000 );
    camera.position.z = 2750;
    scene = new THREE.Scene();
    var segments = 10000;
    var geometry = new THREE.BufferGeometry();
    var material = new THREE.LineBasicMaterial({ vertexColors: THREE.VertexColors });
    var positions = new Float32Array( segments * 3 );
    var colors = new Float32Array( segments * 3 );
    var r = 800;
    for ( var i = 0; i < segments; i ++ ) {
        var x = Math.random() * r - r / 2;
        var y = Math.random() * r - r / 2;
        var z = Math.random() * r - r / 2;
        positions[ i * 3 ] = x;
        positions[ i * 3 + 1 ] = y;
        positions[ i * 3 + 2 ] = z;
        colors[ i * 3 ] = ( x / r ) + 0.5;
        colors[ i * 3 + 1 ] = ( y / r ) + 0.5;
        colors[ i * 3 + 2 ] = ( z / r ) + 0.5;
    }
    geometry.addAttribute( 'position', new THREE.BufferAttribute( positions, 3 ) );
    geometry.addAttribute( 'color', new THREE.BufferAttribute( colors, 3 ) );
    geometry.computeBoundingSphere();
    mesh = new THREE.Line( geometry, material );
    scene.add( mesh );
    renderer = new THREE.Canvas3DRenderer({ canvas: canvas, antialias: true, devicePixelRatio: canvas.devicePixelRatio });
    renderer.setClearColor( 0x707070 );
    renderer.setSize(canvas.width, canvas.height);
    renderer.gammaInput = true;
    renderer.gammaOutput = true;
}
function resizeGL(canvas) {
    camera.aspect = canvas.width / canvas.height;
    camera.updateProjectionMatrix();
    renderer.setPixelRatio(canvas.devicePixelRatio);
    renderer.setSize(canvas.width, canvas.height);
}
function paintGL(canvas) {
    var time = Date.now() * 0.001;
    mesh.rotation.x = time * 0.25;
    mesh.rotation.y = time * 0.5;
    renderer.render(scene, camera);
}
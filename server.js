const fs = require('fs')
const express = require('express')
const app = express()
const multer  = require('multer')
const upload = multer({ dest: 'tmp-uploads/' })
const uuidv4 = require('uuid/v4')

const port = 3000

let images = []

app.get('/api/search', (req, res) => {
  res.setHeader('Content-Type', 'application/json');
  res.send(JSON.stringify(images));
})

app.post('/api/image', upload.array('files[]'), (req, res) => {
  const files = req.files.map(file => {
    return { "id": uuidv4(), "src": "/uploads/" + file.originalname, tags: [file.mimetype] }
  })
  req.files.forEach(file => {
    fs.renameSync(file.path, "uploads/" + file.originalname)
  })
  images = images.concat(files)
  res.send(200)
})

app.use(express.static('client/dist'))
app.use("/uploads", express.static(__dirname + "/uploads"));

app.listen(port, () => console.log(`Example app listening on port ${port}!`))